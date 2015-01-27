package controllers

import globals.{ApiEndpoint, Token}
import models.{Author, Talk}
import play.api.Logger
import play.api.Play.current
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc._
import monitoring.Monitoring._

import scala.concurrent.Future

object Application extends Controller {

  def index = Action.async {
    val futureCategories = categories()
    val futureProducts = products()
    for {
      c ← futureCategories
      p ← futureProducts
    } yield {
      Ok(views.html.index("talk", c, p))
    }
  }

  def categories(): Future[Seq[String]] =
    for {
      ws ← Token.withToken(WS.url(s"${ApiEndpoint.baseUrl}/categories"))
      response ← performance(ws.get())
    } yield {
      val json = response.json
      (json \ "results").as[JsArray].value.map(r ⇒ (r \ "name" \ "en").as[String])
    }

  def newTalk(id: String,
              title: String,
              attributes: Option[Seq[Attribute]]): Talk = {
    val att = attributes.getOrElse(Nil).groupBy(_.name).mapValues(_.apply(0)).mapValues(_.value)

    Logger.debug(att.mkString)

    val authorName = att.get("Author").collect { case JsString(s) ⇒ s }
    val twitter = att.get("twitter").collect { case JsString(s) ⇒ s }
    val slides = att.get("slides").collect { case JsArray(a) ⇒ a.collect { case JsString(s) ⇒ s } }
    val videos = att.get("videos").collect { case JsArray(a) ⇒ a.collect { case JsString(s) ⇒ s } }

    val author = Author(name = authorName, twitter = twitter)

    Talk(id, title, author, videos, slides)
  }

  case class Attribute(name: String, value: JsValue)
  object Attribute {
    implicit val jsonReads = Json.reads[Attribute]
  }

  case class SetAttribute(name: String, value: Seq[String])
  object SetAttribute {
    implicit val jsonReads = Json.reads[SetAttribute]
  }

  implicit val talkReads: Reads[Talk] = (
    (__ \ "id").read[String] and
    (__ \ "masterData" \ "current" \ "name" \ "en").read[String] and
    (__ \ "masterData" \ "current" \ "masterVariant" \ "attributes").readNullable[Seq[Attribute]]
  )(newTalk _)

  def products(): Future[Seq[Talk]] =
    for {
      ws ← Token.withToken(WS.url(s"${ApiEndpoint.baseUrl}/products"))
      response ← performance(ws.get())
    } yield {
      val json = response.json
//      Logger.debug(Json.prettyPrint(json))
      (json \ "results").as[Seq[Talk]]
    }


}