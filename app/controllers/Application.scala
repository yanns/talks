package controllers

import globals.{ApiEndpoint, Token}
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.JsArray
import play.api.libs.ws.WS
import play.api.mvc._
import monitoring.Monitoring._

object Application extends Controller {

  def index = Action.async {
    for {
      ws ← Token.withToken(WS.url(s"${ApiEndpoint.baseUrl}/categories"))
      response ← performance(ws.get())
    } yield {
      val json = response.json
      val names = (json \ "results").as[JsArray].value.map(r ⇒ (r \ "name" \ "en").as[String])
      Ok(views.html.index("talk", names))
    }
  }

}