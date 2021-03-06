package web

import generator.{PasswordGenerator, Signal, Var}
import org.scalajs.dom
import org.scalajs.dom.html
import dom.document
import scala.scalajs.js

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
      passwordList.foreach(passwords => generatedList.textContent = passwords.reduce((p1, p2) => p1 + "\n" + p2))
    }
  }
}
