package globals

import java.util.Date
import java.util.concurrent.atomic.AtomicReference

import play.api.Logger
import play.api.Play.current
import play.api.http.Status._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.{EmptyBody, WSRequestHolder, WS}
import play.api.libs.ws.WSAuthScheme.BASIC

import scala.concurrent.{Future, Promise}
import scala.util.Success

object Token {

  case class Token(token: String, nextUpdate: Long)

  private val currentToken = new AtomicReference[Future[Token]](null)

  private val ENSURED_TIME_TO_LIVE = 5 * 60L * 1000L

  def withToken(ws: WSRequestHolder): Future[WSRequestHolder] =
    getToken() map (token ⇒ ws.withHeaders("Authorization" → s"Bearer $token"))


  def getToken(): Future[String] = {
    Option(currentToken.get) match {
      case Some(future) ⇒
        future.value match {

          case Some(Success(token)) if token.nextUpdate > System.currentTimeMillis() + ENSURED_TIME_TO_LIVE ⇒
            Future.successful(token.token)

          case Some(_) ⇒
            refreshToken().map(_.token)

          case None ⇒
            future.map(_.token)
        }
      case None ⇒
        refreshToken().map(_.token)
    }
  }

  private def refreshToken(): Future[Token] = {
    val prev = currentToken.get()
    val next = Promise[Token]()

    if (currentToken.compareAndSet(prev, next.future)) {
      Logger.debug("refreshing OAuth token")
      next completeWith {
        val clientId = config("sphereio.clientId")
        val clientSecret = config("sphereio.clientSecret")
        val projectKey = config("sphereio.projectKey")
        val authUrl = config("sphereio.authUrl")
        WS.url(s"$authUrl/oauth/token")
          .withQueryString("grant_type" -> "client_credentials", "scope" -> s"manage_project:$projectKey")
          .withAuth(clientId, clientSecret, BASIC)
          .withBody(EmptyBody)
          .execute("POST")
          .map { response ⇒

            response.status match {
              case OK ⇒
                val json = response.json
                val token = (json \ "access_token").as[String]
                val expirationInSeconds = (json \ "expires_in").as[Long]
                val nextUpdate = new Date().getTime + expirationInSeconds * 1000
                Token(token, nextUpdate)
              case badStatus ⇒ throw new Exception(s"impossible to create a token [status=$badStatus, body=${response.body}]")
          }
        }
      }
    }

    currentToken.get()
  }

  private def config(key: String): String =
    Option(System.getenv(key)).getOrElse(
      current.configuration.getString(key).getOrElse(throw new Exception(s"'$key' should be configured")))
}
