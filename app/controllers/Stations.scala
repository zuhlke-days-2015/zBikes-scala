package controllers

import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc._

object InMemoryState {
  import Model._

  var stations = Map.empty[StationId, Station]
  var bikes = Map.empty[BikeId, StationId]
}

object Model {
  type StationId = String
  type BikeId = String

  case class Location(lat: Double, long: Double)
  case class Station(name: String, location: Location)

  implicit val locationFormat = Json.format[Location]
  implicit val stationFormat = Json.format[Station]
}

class Stations extends Controller {
  import Model._

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
        Json.toJson(station).as[JsObject]
        ++ Json.obj("availableBikes" -> InMemoryState.bikes.filter(byStationId(id)).keys)
      )
      case None => NotFound
    }
  }
}

