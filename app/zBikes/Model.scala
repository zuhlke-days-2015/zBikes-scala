package zBikes

import play.api.libs.json.{JsObject, Json}
import reactivemongo.bson.BSONObjectID

object Model {
  import Json._

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

  case class Station(id: String, name: String, location: Location)

  implicit val locationFormat = format[Location]
  implicit val stationReads = reads[Station]
  implicit val stationWrites = writes[Station].transform(js => js.as[JsObject] - "id")
}
