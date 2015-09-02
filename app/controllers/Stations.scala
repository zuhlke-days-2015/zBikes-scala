package controllers

import play.api.libs.json.{JsValue, Json}
import play.api.mvc._

class Stations extends Controller {

  def upsert(id: String) = Action(BodyParsers.parse.json) { implicit req =>
    InMemoryState.stations = InMemoryState.stations + (id -> req.body)
    Ok.withHeaders("Location" -> routes.Stations.view(id).url)
  }

  def view(id: String) = Action {
    InMemoryState.stations.get(id) match {
      case Some(station) => Ok(station)
      case None => NotFound
    }
  }
}

object InMemoryState {
  var stations = Map.empty[String, JsValue]
}