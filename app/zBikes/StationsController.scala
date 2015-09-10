package zBikes

import play.api.Play
import play.api.libs.concurrent.Execution
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc.{Action, BodyParsers, Controller}

import scala.collection.immutable.SortedMap

class StationsController extends Controller {

  import Execution.Implicits.defaultContext
  import Play.current
  import Model._
  import JsonFormatters._
  import Json._

  val persistence = InMemoryState
  import persistence._

  def upsert(stationId: String) = Action(BodyParsers.parse.json) { implicit req =>
    val station = req.body.as[Station]
    val bikes = (req.body \ "availableBikes").as[Seq[BikeId]]

    InMemoryState.upsert(stationId -> station)
    bikeStore = bikeStore.filterNot(availableAt(stationId))
    for (bikeId <- bikes) {
      bikeStore += (bikeId -> Available(stationId))
    }
    Ok.withHeaders("Location" -> routes.StationsController.view(stationId).url)
  }

  def view(stationId: String) = Action {
    stationStore.get(stationId) match {
      case Some(station) => Ok(
        toJson(station).as[JsObject]
          ++ obj("availableBikes" -> bikeStore.filter(availableAt(stationId)).keys.toSeq.sorted)
      )
      case None => NotFound
    }
  }

  def near(lat: Double, long: Double) = Action {
    val localStations = stationStore.filter(nearTo(Location(lat, long)))
    Ok(Json.obj("items" -> localStations.map { case (stationId, station) =>
      toJson(station).as[JsObject] ++ obj(
        "availableBikeCount" -> bikeStore.count(availableAt(stationId)),
        "selfUrl" -> routes.StationsController.view(stationId),
        "hireUrl" -> routes.StationsController.hireBike(stationId)
      )
    }))
  }

  val removeAll = Action {
    stationStore = Map.empty
    bikeStore = SortedMap.empty
    Ok
  }

  def hireBike(stationId: String) = Action.async(parse.json) { req =>
    val username = (req.body \ "username").as[String]
    WS.url(s"http://localhost:9005/customer/$username").get().map(_.status).map {
      case OK =>
        bikeStore.find(availableAt(stationId)) match {
          case Some((availableBikeId, _)) =>
            bikeStore += (availableBikeId -> Hired(username))
            Ok(obj("bikeId" -> availableBikeId))
          case None =>
            NotFound
        }
      case UNAUTHORIZED => Unauthorized
      case other => InternalServerError
    }
  }

  def returnBike(stationId: StationId, bikeId: BikeId) = Action(parse.json) { req =>
    val username = (req.body \ "username").as[String]
    bikeStore.get(bikeId) match {
      case None => NotFound
      case Some(Hired(`username`)) =>
        bikeStore += (bikeId -> Available(stationId))
        Ok
      case Some(Hired(otherUsername)) =>
        Forbidden
      case Some(Available(_)) =>
        Conflict
    }
  }

  def depleted = Action {
    val depletedStations = stationStore flatMap { case (depletedStationId, depletedStation) =>
      val count = bikeStore.count(availableAt(depletedStationId))
      if (count > 10) None
      else {
        val nearbyStations = stationStore.filter(nearTo(depletedStation.location))
        val nearbyFullStations = nearbyStations.flatMap { case (stationId, station) =>
          val count = bikeStore.count(availableAt(stationId))
          if (count > 20) Some(stationId -> count)
          else None
        }
        Some(depletedStationId ->(count, nearbyFullStations))
      }
    }

    val stationUrlAndAvailableBikes = (stationId: StationId, bikeCount: Int) => Json.obj(
      "stationUrl" -> routes.StationsController.view(stationId),
      "availableBikes" -> bikeCount
    )

    Ok(Json.obj("items" -> depletedStations.map { case (stationId, (bikeCount, nearbyFullStations)) =>
      stationUrlAndAvailableBikes(stationId, bikeCount) ++ Json.obj("nearbyFullStations" -> nearbyFullStations.map(stationUrlAndAvailableBikes.tupled))
    }))
  }
}