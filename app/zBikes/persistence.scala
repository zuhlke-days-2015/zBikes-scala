package zBikes

import play.api.Play
import play.api.libs.concurrent.Execution
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.{CursorProducer, ReadPreference}
import reactivemongo.api.collections.bson.BSONCollection
import zBikes.Mongo.Bikes.ReturnResult.HiredByOtherUser

import scala.collection.immutable.SortedMap
import scala.concurrent.Future

object Mongo {
  import Execution.Implicits.defaultContext
  import Model._
  import reactivemongo.bson._

  lazy val mongo = Play.current.injector.instanceOf[ReactiveMongoApi]

  object Stations {
    private val collection = mongo.connection.db("zBikes").collection[BSONCollection]("station")
    private implicit val locationFormat = Macros.handler[Location]
    private implicit val stationHandler = Macros.handler[Station]

    def upsert(s: Station) = collection.update(
      selector = BSONDocument("_id" -> s.id),
      update = s,
      upsert = true
    )

    def find(id: StationId): Future[Option[Station]] = collection.find(BSONDocument("_id" -> id)).one[Station]

    def findNear(l: Location): Future[List[Station]] = collection.find(
      BSONDocument(
        "$and" -> Seq(
          BSONDocument("location.lat" -> BSONDocument("$lte" -> (l.lat + 0.011))),
          BSONDocument("location.lat" -> BSONDocument("$gte" -> (l.lat - 0.011))),
          BSONDocument("location.long" -> BSONDocument("$lte" -> (l.long + 0.011))),
          BSONDocument("location.long" -> BSONDocument("$gte" -> (l.long - 0.011)))
        )
      )
    ).cursor[Station](ReadPreference.primary).collect[List]()

    def removeAll() = collection.drop()
  }


  object Bikes {
    private val collection = mongo.connection.db("zBikes").collection[BSONCollection]("bike")

    implicit val availableReader = Macros.handler[Available]
    implicit val hiredReader = Macros.handler[Hired]

    implicit val bikeStatusReader = new BSONDocumentReader[Bike] {
      def read(bson: BSONDocument) =
        availableReader.readOpt(bson) orElse
        hiredReader.readOpt(bson) getOrElse
        (throw new IllegalStateException(s"Bike stored as $bson"))
    }

    def removeAll() = collection.drop()

    def remove(stationId: String, bikes: Seq[BikeId]) = collection.remove(
      query = BSONDocument("$or" -> Seq(
        BSONDocument("atStation" -> stationId),
        BSONDocument("_id" -> BSONDocument("$in" -> bikes))
      ))
    )

    def insert(stationId: String, bikes: Seq[BikeId]) = collection.bulkInsert(
      documents = bikes.map( id =>
        BSONDocument("atStation" -> stationId, "_id" -> id)
      ).toStream,
      ordered = false
    )

    def findAll(stationIds: List[StationId]): Future[List[Available]] =
      collection.find(BSONDocument("atStation" -> BSONDocument("$in" -> stationIds))).cursor[Available].collect[List]()

    def hireBike(username: String, stationId: StationId): Future[Option[BikeId]] = collection.findAndUpdate(
      selector = BSONDocument("atStation" -> stationId),
      update = BSONDocument(
        "$unset" -> BSONDocument("atStation" -> 1),
        "$set" -> BSONDocument("withUser" -> username)
      ),
      fetchNewObject = true
    ).map(_.value.flatMap(_.getAs[BikeId]("_id")))

    def returnBike(username: String, stationId: StationId, bikeId: BikeId) = collection.findAndUpdate(
      selector = BSONDocument("_id" -> bikeId, "withUser" -> username),
      update = BSONDocument("$set" -> BSONDocument("atStation" -> stationId))
    ).flatMap { e =>
      import ReturnResult._
      if (e.lastError.exists(_.updatedExisting)) Future.successful(Completed)
      else collection.find(BSONDocument("_id" -> bikeId)).one[Bike].map {
        case Some(Hired(_, otherUser)) => HiredByOtherUser
        case Some(Available(_, station)) => CurrentlyAtStation
        case None => BikeNotFound
      }
    }

    sealed trait ReturnResult
    object ReturnResult {
      case object Completed
      case object BikeNotFound
      case object HiredByOtherUser
      case object CurrentlyAtStation
    }
  }
}