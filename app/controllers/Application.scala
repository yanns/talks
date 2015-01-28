package controllers

import globals.{ApiEndpoint, Token}
import models.{Category, Author, Talk}
import play.api.Logger
import play.api.Play.current
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.{WSRequestHolder, WS}
import play.api.mvc._
import monitoring.Monitoring._

import scala.concurrent.Future

object Application extends Controller {

  def index = main(None)

  def forCategory(categoryId: String, slug: String) = main(Some(categoryId))

  def main(categoryId: Option[String]) = Action.async {
    val futureCategories = categories()
    val futureProducts = products(categoryId)
    for {
      c ← futureCategories
      p ← futureProducts
    } yield {
      Ok(views.html.index("talk", c, p))
    }
  }

  implicit val categoryReads: Reads[Category] = (
    (__ \ "id").read[String] and
    (__ \ "name" \ "en").read[String] and
    (__ \ "slug" \ "en").read[String]
  )(Category.apply _)


  def categories(): Future[Seq[Category]] =
    for {
      ws ← Token.withToken(WS.url(s"${ApiEndpoint.baseUrl}/categories"))
      response ← performance(ws.get())
    } yield {
      val json = response.json
      (json \ "results").as[JsArray].value.map(_.as[Category])
    }

  def newTalk(id: String,
              title: String,
              attributes: Option[Seq[Attribute]]): Talk = {
    val att = attributes.getOrElse(Nil).groupBy(_.name).mapValues(_.apply(0)).mapValues(_.value)

    Logger.debug(att.mkString(", "))

    val authorName = att.get("Author").collect { case JsString(s) ⇒ s}
    val twitter = att.get("twitter").collect { case JsString(s) ⇒ s}
    val slides = att.get("slides").collect { case JsArray(a) ⇒ a.collect { case JsString(s) ⇒ s}}
    val videos = att.get("videos").collect { case JsArray(a) ⇒ a.collect { case JsString(s) ⇒ s}}

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
      (__ \ "name" \ "en").read[String] and
      (__ \ "masterVariant" \ "attributes").readNullable[Seq[Attribute]]
    )(newTalk _)

  def products(categoryId: Option[String]): Future[Seq[Talk]] = {
    def filterByCategory(ws: WSRequestHolder): WSRequestHolder =
      categoryId.fold(ws) { id ⇒ ws.withQueryString("filter.query" → s"""categories.id:"$id"""")}

    for {
      ws ← Token.withToken(
        filterByCategory(
          WS.url(s"${ApiEndpoint.baseUrl}/product-projections/search")
            .withQueryString("staged" → "true")))
      response ← performance(ws.get())
    } yield {
      val json = response.json
      Logger.trace(Json.prettyPrint(json))
      (json \ "results").as[Seq[Talk]]
    }
  }


}