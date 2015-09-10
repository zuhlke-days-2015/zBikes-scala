package zBikes

import play.api.Play
import play.api.libs.concurrent.Execution
import play.modules.reactivemongo.ReactiveMongoApi
import play.modules.reactivemongo.json.ImplicitBSONHandlers.BSONDocumentWrites
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.{BSONDocumentWriter, BSONDocument}
import zBikes.Model.{Station, StationId}

import scala.collection.immutable.SortedMap
import scala.concurrent.Future

import scala.concurrent.Future

trait Persistence {
  import Model._

  def upsert(station: (StationId, Station)): Future[_]
}

object Mongo extends Persistence {

  import Execution.Implicits.defaultContext

  lazy val mongo = Play.current.injector.instanceOf[ReactiveMongoApi]

  implicit val stationFormat = new BSONDocumentWriter[Station] {
    def write(t: Station) = BSONDocument(
      "name" -> t.name
    )
  }

  def upsert(s: (StationId, Station)) = s match { case (id, station) =>
    mongo.connection.db("zBikes").collection[BSONCollection]("station").update(
      selector = BSONDocument("_id" -> id),
      update = station,
      upsert = true
    )
  }
}

object InMemoryState extends Persistence {
  import Model._

  var stationStore = Map.empty[StationId, Station]
  var bikeStore = SortedMap.empty[BikeId, BikeStatus]

  def upsert(station: (StationId, Station)) = Future.successful(stationStore += station)

  def availableAt(stationId: String): PartialFunction[(BikeId, BikeStatus), Boolean] = {
    case (_, Available(id)) => stationId == id
    case _ => false
  }

  def nearTo(otherLocation: Location): ((StationId, Station)) => Boolean = {
    case (_, otherStation) => otherStation.location near otherLocation
  }
}
