package controllers

import play.api.Play
import play.api.libs.concurrent.Execution
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc._
import Json._

import scala.collection.immutable.SortedMap


object InMemoryState {
  import Model._

  var stations = Map.empty[StationId, Station]
  var bikes = SortedMap.empty[BikeId, BikeStatus]
}

object Model {
  type StationId = String
  type BikeId = String

  sealed trait BikeStatus
  case class Hired(username: String) extends BikeStatus
  case class Available(stationId: StationId) extends BikeStatus

  case class Location(lat: Double, long: Double) {
    def near(other: Location) = {
      val tolerance = 0.011
      other.lat <= lat + tolerance &&
      other.lat >= lat - tolerance &&
      other.long <= long + tolerance &&
      other.long >= long - tolerance
    }
  }

  case class Station(name: String, location: Location)

  implicit val locationFormat = format[Location]
  implicit val stationFormat = format[Station]
}

object JsonFormatters {
  implicit val callWrites: Writes[Call] = Writes[Call](c => JsString(c.url))
}

class Stations extends Controller {
  import Model._
  import JsonFormatters._
  import Play.current
  import Execution.Implicits.defaultContext

  def upsert(stationId: String) = Action(BodyParsers.parse.json) { implicit req =>
    val station = req.body.as[Station]
    val bikes = (req.body \ "availableBikes").as[Seq[BikeId]]

    InMemoryState.stations += (stationId -> station)
    InMemoryState.bikes = InMemoryState.bikes.filterNot(availableAt(stationId))
    for (bikeId <- bikes) {
      InMemoryState.bikes += (bikeId -> Available(stationId))
    }
    Ok.withHeaders("Location" -> routes.Stations.view(stationId).url)
  }

  def view(stationId: String) = Action {
    InMemoryState.stations.get(stationId) match {
      case Some(station) => Ok(
        toJson(station).as[JsObject]
        ++ obj("availableBikes" -> InMemoryState.bikes.filter(availableAt(stationId)).keys.toSeq.sorted)
      )
      case None => NotFound
    }
  }

  def near(lat: Double, long: Double) = Action {
    val location = Location(lat, long)
    val localStations = InMemoryState.stations.filter { case (_, station)  =>
      station.location near location
    }
    Ok(Json.obj("items" -> localStations.map { case (stationId, station) =>
      toJson(station).as[JsObject] ++ obj(
        "availableBikeCount" -> InMemoryState.bikes.count(availableAt(stationId)),
        "selfUrl" -> routes.Stations.view(stationId),
        "hireUrl" -> routes.Stations.hireBike(stationId)
      )
    }))
  }

  val removeAll = Action {
    InMemoryState.stations = Map.empty
    InMemoryState.bikes = SortedMap.empty
    Ok
  }

  def hireBike(stationId: String) = Action.async(parse.json) { req =>
    val username = (req.body \ "username").as[String]
    WS.url(s"http://localhost:9005/customer/$username").get().map(_.status).map {
      case OK =>
        InMemoryState.bikes.find(availableAt(stationId)) match {
          case Some((availableBikeId, _)) =>
            InMemoryState.bikes += (availableBikeId -> Hired(username))
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
    InMemoryState.bikes.get(bikeId) match {
      case None => NotFound
      case Some(Hired(`username`)) =>
        InMemoryState.bikes += (bikeId -> Available(stationId))
        Ok
      case Some(Hired(otherUsername)) =>
        Forbidden
      case Some(Available(_)) =>
        Conflict
    }
  }

  private def availableAt(stationId: String): PartialFunction[(BikeId, BikeStatus), Boolean] = {
    case (_, Available(id)) => stationId == id
    case _ => false
  }
}

