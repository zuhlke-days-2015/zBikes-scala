package controllers

import play.api.libs.json._
import play.api.mvc._
import Json._

object InMemoryState {
  import Model._

  var stations = Map.empty[StationId, Station]
  var bikes = Map.empty[BikeId, StationId]
}

object Model {
  type StationId = String
  type BikeId = String

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

  def upsert(id: String) = Action(BodyParsers.parse.json) { implicit req =>
    val station = req.body.as[Station]
    val bikes = (req.body \ "availableBikes").as[Seq[BikeId]]

    InMemoryState.stations += (id -> station)
    InMemoryState.bikes = InMemoryState.bikes.filterNot(byStationId(id))
    for (bikeId <- bikes) {
      InMemoryState.bikes += (bikeId -> id)
    }
    Ok.withHeaders("Location" -> routes.Stations.view(id).url)
  }

  def byStationId(id: String): PartialFunction[(BikeId, StationId), Boolean] = { case (_, stationId) => stationId == id }

  def view(id: String) = Action {
    InMemoryState.stations.get(id) match {
      case Some(station) => Ok(
        toJson(station).as[JsObject]
        ++ obj("availableBikes" -> InMemoryState.bikes.filter(byStationId(id)).keys.toSeq.sorted)
      )
      case None => NotFound
    }
  }

  def near(lat: Double, long: Double) = Action {
    val location = Location(lat, long)
    val localStations = InMemoryState.stations.filter { case (_, station)  =>
      station.location near location
    }
    Ok(Json.obj("items" -> localStations.map { case (id, station) =>
      toJson(station).as[JsObject] ++ obj(
        "availableBikeCount" -> InMemoryState.bikes.count(byStationId(id)),
        "hireUrl" -> routes.Stations.hireBike(id)
      )
    }))
  }

  val removeAll = Action {
    InMemoryState.stations = Map.empty
    InMemoryState.bikes = Map.empty
    Ok
  }

  def hireBike(id: String) = Action {
    Ok
  }
}

