package zBikes

import play.api.libs.json.{JsString, Writes}
import play.api.mvc.Call

object JsonFormatters {
  implicit val callWrites: Writes[Call] = Writes[Call](c => JsString(c.url))
}
