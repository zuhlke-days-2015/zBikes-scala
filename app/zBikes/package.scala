import play.api.libs.json.{JsString, Writes}
import play.api.mvc.Call

package object zBikes {
  implicit val callWrites: Writes[Call] = Writes[Call](c => JsString(c.url))
}
