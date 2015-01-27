package monitoring

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future

object Monitoring {

  def performance[A](f: Future[A]): Future[A] = {
    val start = System.currentTimeMillis()
    f andThen { case _ ⇒ Logger.debug(humanDuration(System.currentTimeMillis() - start)) }
  }

  private def humanDuration(duration: Long): String = s"$duration ms"
}
