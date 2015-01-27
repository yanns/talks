package globals

import play.api.Play._

object ApiEndpoint {

  lazy val projectKey = config("sphereio.projectKey")
  lazy val baseUrl = s"https://api.sphere.io/$projectKey"

  private def config(key: String): String =
    current.configuration.getString(key).getOrElse(throw new Exception(s"'$key' should be configured"))
}
