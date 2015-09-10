package zBikes

import play.api.Play
import play.api.libs.concurrent.Execution
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.{CursorProducer, ReadPreference}
import reactivemongo.api.collections.bson.BSONCollection

import scala.collection.immutable.SortedMap
import scala.concurrent.Future

object Mongo {


  import Execution.Implicits.defaultContext
  import Model._
  import reactivemongo.bson._

  lazy val mongo = Play.current.injector.instanceOf[ReactiveMongoApi]
  private val stations = mongo.connection.db("zBikes").collection[BSONCollection]("station")

  implicit val locationFormat = Macros.handler[Location]
  implicit val stationHandler = Macros.handler[Station]

  def upsertStation(s: Station) = stations.update(
    selector = BSONDocument("_id" -> s.id),
    update = s,
    upsert = true
  )

  def findStation(id: StationId): Future[Option[Station]] = stations.find(BSONDocument("_id" -> id)).one[Station]

  def findStationsNear(l: Location): Future[List[Station]] = stations.find(
    BSONDocument(
      "$and" -> Seq(
        BSONDocument("location.lat" -> BSONDocument("$lte" -> (l.lat + 0.011))),
        BSONDocument("location.lat" -> BSONDocument("$gte" -> (l.lat - 0.011))),
        BSONDocument("location.long" -> BSONDocument("$lte" -> (l.long + 0.011))),
        BSONDocument("location.long" -> BSONDocument("$gte" -> (l.long - 0.011)))
      )
    )
  ).cursor[Station](ReadPreference.primary).collect[List]()

  def removeAllStations(): Future[Unit] = stations.drop()

  @deprecated
  def allStations: Future[List[Station]] = stations.find(BSONDocument()).cursor[Station](ReadPreference.primary).collect[List]()
}

object InMemoryState {
  import Model._

  var bikeStore = SortedMap.empty[BikeId, BikeStatus]

  def availableAt(stationId: String): PartialFunction[(BikeId, BikeStatus), Boolean] = {
    case (_, Available(id)) => stationId == id
    case _ => false
  }

  def nearTo(otherLocation: Location): Station => Boolean = { _.location near otherLocation }
}
