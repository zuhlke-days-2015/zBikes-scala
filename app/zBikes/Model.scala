package zBikes

import play.api.libs.json.{JsObject, Json}
import reactivemongo.bson.BSONObjectID

object Model {
  import Json._

  type StationId = String
  type BikeId = String

  sealed trait Bike {
    def _id: BikeId
  }
  case class Hired(_id: BikeId, withUser: String) extends Bike
  case class Available(_id: BikeId, atStation: StationId) extends Bike

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
  implicit val stationWrites = writes[Station].transform(js => js.as[JsObject] - "id")
}
