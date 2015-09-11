package zBikes

import play.api.Play
import play.api.libs.concurrent.Execution
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc.{Action, BodyParsers, Controller}

import scala.collection.immutable.SortedMap
import scala.concurrent.Future

class StationsController extends Controller {

  import Execution.Implicits.defaultContext
  import Play.current
  import Model._
  import Json._

  def upsert(stationId: String) = Action.async(parse.json) { implicit req =>
    val station = Station(
      id = stationId,
      name = (req.body \ "name").as[String],
      location = (req.body \ "location").as[Location]
    )
    val bikeIds = (req.body \ "availableBikes").as[Seq[BikeId]]

    for {
      _ <- Mongo.Stations.upsert(station)
      _ <- Mongo.Bikes.removeAll(stationId, bikeIds)
      _ <- Mongo.Bikes.insert(stationId, bikeIds)
    } yield Ok.withHeaders("Location" -> routes.StationsController.view(stationId).url)
  }



  def view(stationId: String) = Action.async {
    Mongo.Bikes.findAll(stationId) flatMap { bikes =>
      Mongo.Stations.find(id = stationId) map {
        case Some(station) =>
          Ok(toJson(station).as[JsObject] ++ obj("availableBikes" -> bikes.map(_._id)))
        case None => NotFound
      }
    }
  }

  def near(lat: Double, long: Double) = Action.async {
    for {
      localStations <- Mongo.Stations.findNear(Location(lat, long))
      bikes <- Mongo.Bikes.findAll(localStations.map(_.id))
    } yield Ok(Json.obj("items" -> localStations.map { station =>
      toJson(station).as[JsObject] ++ obj(
        "availableBikeCount" -> bikes.count(_.atStation == station.id),
        "selfUrl" -> routes.StationsController.view(station.id),
        "hireUrl" -> routes.StationsController.hireBike(station.id)
      )
    }))
  }


  val removeAll = Action.async {
    for {
      _ <- Mongo.Stations.removeAll()
      _ <- Mongo.Bikes.removeAll()
    } yield Ok
  }

  def hireBike(stationId: StationId) = Action.async(parse.json) { req =>
    val username = (req.body \ "username").as[String]
    WS.url(s"http://localhost:9005/customer/$username").get().map(_.status).flatMap {
      case OK => Mongo.Bikes.hireFrom(stationId) map {
        case Some(bikeId) => Ok(obj("bikeId" -> bikeId))
        case None => NotFound
      }
      case UNAUTHORIZED => Future.successful(Unauthorized)
      case other => Future.successful(BadGateway)
    }
  }

  def returnBike(stationId: StationId, bikeId: BikeId) = Action.async(parse.json) { req =>
//    val username = (req.body \ "username").as[String]
//    InMemoryState.bikeStore.get(bikeId) match {
//      case None => NotFound
//      case Some(Hired(`username`)) =>
//        InMemoryState.bikeStore += (bikeId -> Available(stationId))
//        Ok
//      case Some(Hired(otherUsername)) =>
//        Forbidden
//      case Some(Available(_)) =>
//        Conflict
//    }

    Future(InternalServerError)
  }

  def depleted = Action.async {
//    Mongo.Stations.allStations.map { stationStore =>
//      val depletedStations = stationStore flatMap { case depletedStation =>
//        val count = Bikes.count(depletedStation.id)
//        if (count > 10) None
//        else {
//          val nearbyStations = stationStore.filter(InMemoryState.nearTo(depletedStation.location))
//          val nearbyFullStations = nearbyStations.flatMap { case station =>
//            val count = Bikes.count(station.id)
//            if (count > 20) Some(station.id -> count)
//            else None
//          }
//          Some(depletedStation.id -> (count, nearbyFullStations))
//        }
//      }
//
//      val stationUrlAndAvailableBikes = (stationId: StationId, bikeCount: Int) => Json.obj(
//        "stationUrl" -> routes.StationsController.view(stationId),
//        "availableBikes" -> bikeCount
//      )
//
//      Ok(Json.obj("items" -> depletedStations.map { case (stationId, (bikeCount, nearbyFullStations)) =>
//        stationUrlAndAvailableBikes(stationId, bikeCount) ++ Json.obj("nearbyFullStations" -> nearbyFullStations.map(stationUrlAndAvailableBikes.tupled))
//      }))
//    }

    Future(InternalServerError)
  }
}