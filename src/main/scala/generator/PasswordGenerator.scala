package generator

import generator.GeneratorUtils._

import scala.concurrent.Future

object PasswordGenerator {

  def generateSecurePasswordList(size: Signal[String], passwordHintText: Signal[String]): Signal[Future[Seq[String]]] = Signal {
    val passwordHint = passwordHintText()
    val listSize = size().toInt
    import scala.concurrent.ExecutionContext.Implicits.global
    val tasks: Seq[Future[String]] = for (_ <- 0 until listSize)
      yield Future(generateSecurePassword(passwordHint))

    val passwords: Future[Seq[String]] = Future.sequence(tasks)
    passwords
  }
}
