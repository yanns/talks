package controllers

import globals.{ApiEndpoint, Token}
import models.Category
import monitoring.Monitoring._
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.mvc.{Action, Controller}
import play.twirl.api.Html

// start common code

import scala.language.implicitConversions

trait ComponentRendering[In] extends (In ⇒ String)
object ComponentRendering {
  implicit def toComponentRendering[In](f: In ⇒ String): ComponentRendering[In] =
    new ComponentRendering[In] {
      override def apply(in: In): String = f(in)
    }
}

trait GraphQLBuilder[A] {
  def fragmentQL: String
  def parse: Reads[A]
}
object GraphQLBuilder {
  def array[A](path: String)(implicit c: GraphQLBuilder[A]) = new GraphQLBuilder[Seq[A]] {
    val fragmentQL = s"$path { ${c.fragmentQL} } "
    val parse = (JsPath \ path).read(Reads.seq(c.parse))
  }

}

case class Component[In](rendering: ComponentRendering[In])(implicit val graphQLBuilder: GraphQLBuilder[In])
object Component {

  def query(c: Component[JsValue]) = new Component[JsValue](c.rendering)(new GraphQLBuilder[JsValue] {
    override val fragmentQL: String = s"query { ${c.graphQLBuilder.fragmentQL} }"
    override def parse: Reads[JsValue] = (JsPath \ "data").read(c.graphQLBuilder.parse)
  })

  def apply(path: String, c: Component[JsValue]) = new Component[JsValue](c.rendering)(new GraphQLBuilder[JsValue] {
    override val fragmentQL: String = s"$path { ${c.graphQLBuilder.fragmentQL} }"
    override def parse: Reads[JsValue] = (JsPath \ path).read(c.graphQLBuilder.parse)
  })

  implicit def toJsValue[A](c1: Component[A]): Component[JsValue] = new Component[JsValue](new ComponentRendering[JsValue] {
    override def apply(js: JsValue): String = {
      val a = c1.graphQLBuilder.parse.reads(js).getOrElse(
        throw new RuntimeException(s"cannot parse ${Json.prettyPrint(js)} for fragment:\n{${c1.graphQLBuilder.fragmentQL}}"))
      c1.rendering(a)
    }
  })(new GraphQLBuilder[JsValue] {
    override def parse: Reads[JsValue] = JsPath.read
    override def fragmentQL: String = c1.graphQLBuilder.fragmentQL
  })

  object DSL {
    implicit def tuppleToComponent[A](c: (String, Component[A])): Component[JsValue] = apply(c._1, c._2)
  }
}

// end common code

// start usage

object Categories extends Controller {

  val oneCategory = (c: Category) ⇒ s"[${c.id}] ${c.name}"

  implicit val oneCategoryQL = new GraphQLBuilder[Category] {
    val fragmentQL: String =
      """id
        |name(locale: "en")
        |slug(locale: "en")
      """.stripMargin

    val parse: Reads[Category] =
      (
        (__ \ "id").read[String] and
        (__ \ "name").read[String] and
        (__ \ "slug").read[String]
      )(Category.apply _)
  }

  val categoryList = (c: Seq[Category]) ⇒
    s"""<ul>
        |${c.map(e ⇒ "<li>" + oneCategory(e) + "</li>").mkString("\n")}
        |</ul>
       """.stripMargin

  val categoryListC = new Component(categoryList)(GraphQLBuilder.array("results"))


  def all = Action.async {
    import Component.DSL._
    val query = Component.query("categories" → categoryListC)
    Logger.info(query.graphQLBuilder.fragmentQL)

    for {
      ws ← Token.withToken(WS.url(s"${ApiEndpoint.baseUrl}/graphql"))
      response ← performance(ws.post(JsObject(Seq("query" → JsString(query.graphQLBuilder.fragmentQL)))))
    } yield {
      val json = response.json
      Logger.info(Json.prettyPrint(json))
      val value = Json.fromJson(json)(query.graphQLBuilder.parse).getOrElse(
        throw new RuntimeException(s"cannot parse ${Json.prettyPrint(json)}"))
      Ok(Html(query.rendering(value)))
    }

  }

}
