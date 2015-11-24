package controllers

import globals.{ApiEndpoint, Token}
import models.Category
import monitoring.Monitoring._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc.{Action, Controller}

import scala.language.implicitConversions

// TODO (YaSi): separate the rendering from the graphql build
case class Component[A](
  fragmentQL: String,
  parse: Reads[A],
  render: A ⇒ String
)

object Categories extends Controller {

  val cat = new Component[Category](
    fragmentQL =
      """id
        |name(locale: "en")
        |slug(locale: "en")
      """.stripMargin,
    parse = {
      val reads: Reads[Category] = (
        (__ \ "id").read[String] and
        (__ \ "name").read[String] and
        (__ \ "slug").read[String]
      )(Category.apply _)
      reads
    },
    render = c ⇒ s"[${c.id}] ${c.name}"
  )

  implicit def toJsValue[A](c1: Component[A]): Component[JsValue] = new Component[JsValue](
    fragmentQL = c1.fragmentQL,
    parse = JsPath.read,
    render = { js ⇒
      val a = c1.parse.reads(js).getOrElse(
        throw new RuntimeException(s"cannot parse ${Json.prettyPrint(js)} for fragment:\n{${c1.fragmentQL}}"))
      c1.render(a)
    }
  )

  def array(path: String, c: Component[JsValue]) = new Component[JsArray](
    fragmentQL = s"$path { ${c.fragmentQL} } ",
    parse = (JsPath \ path).read(Reads.JsArrayReads),
    render = list ⇒ list.value.map(e ⇒ c.render(e)).mkString("\n")
  )

  def path(path: String, c: Component[JsValue]) = new Component[JsValue](
    fragmentQL = s"$path { ${c.fragmentQL} }",
    parse = (JsPath \ path).read(c.parse),
    render = c.render
  )

  def query(c: Component[JsValue]) = new Component[JsValue](
    fragmentQL = s"query { ${c.fragmentQL} }",
    parse = (JsPath \ "data").read(c.parse),
    render = c.render
  )

  def all = Action.async {
    val catQuery = query(path("categories", array("results", cat)))

    for {
      ws ← Token.withToken(WS.url(s"${ApiEndpoint.baseUrl}/graphql"))
      response ← performance(ws.post(JsObject(Seq("query" → JsString(catQuery.fragmentQL)))))
    } yield {
      val json = response.json
      val value = Json.fromJson(json)(catQuery.parse).getOrElse(
        throw new RuntimeException(s"cannot parse ${Json.prettyPrint(json)}"))
      Ok(catQuery.fragmentQL + "\n\n" + catQuery.render(value))
    }

  }

}
