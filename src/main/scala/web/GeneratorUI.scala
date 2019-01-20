package web

import generator.{PasswordGenerator, Signal, Var}
import org.scalajs.dom
import org.scalajs.dom.html
import dom.document
import scala.scalajs.js
import scala.util.{Failure, Success}

object GeneratorUI {
  def main(args: Array[String]): Unit = {
    try {
      setupPasswordGenerator()
    } catch {
      case th: Throwable =>
        th.printStackTrace()
    }
  }

  // Helpers

  def elementById[A <: js.Any](id: String): A =
    document.getElementById(id).asInstanceOf[A]

  def elementValueSignal(element: html.Element, getValue: () => String): Signal[String] = {
    var prevVal = getValue()
    val value = new Var(prevVal)
    val onChange = { (event: dom.Event) =>
      // Reconstruct manually the optimization at the root of the graph
      val newVal = getValue()
      if (newVal != prevVal) {
        prevVal = newVal
        value() = newVal
      }
    }
    element.addEventListener("change", onChange)
    element.addEventListener("keypress", onChange)
    element.addEventListener("keyup", onChange)
    value
  }

  def inputValueSignal(inputID: String): Signal[String] = {
    val inputField = elementById[html.Input](inputID)
    elementValueSignal(inputField, () => inputField.value)
  }

  def textAreaValueSignal(textAreaID: String): Signal[String] = {
    val textArea = elementById[html.TextArea](textAreaID)
    elementValueSignal(textArea, () => textArea.value)
  }

  private lazy val ClearCssClassRegExp =
    new js.RegExp(raw"""(?:^|\s)has-error(?!\S)""", "g")

  // PASSWORD GENERATOR

  def setupPasswordGenerator(): Unit = {
    val passwordHintText = textAreaValueSignal("passwordhinttext")
    val passwordsListSize = inputValueSignal("passwordslistsize")

    val generatedList =
      document.getElementById("generatedlist").asInstanceOf[html.TextArea]

    generatedList.disabled = true
    val generatedPasswordList = PasswordGenerator.generateSecurePasswordList(passwordsListSize, passwordHintText)
    Signal {
      generatedList.rows = passwordsListSize().toInt
      val passwordList = generatedPasswordList()
      import scala.concurrent.ExecutionContext.Implicits.global
      passwordList.onComplete({
        case Success(passwords) =>
          generatedList.textContent = ""
          passwords.foreach{
            password =>
              if (passwords.indexOf(password) == passwords.size-1)
                generatedList.textContent += password
              else
                generatedList.textContent += password + "\n"
          }
        case Failure(exception) =>
          generatedList.textContent += s"An error has occured: ${exception.getMessage}"
      })
    }
  }
}
