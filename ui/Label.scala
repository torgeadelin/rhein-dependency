package rhein.ui
import rhein._
import scala.scalajs.js
import scalatags.JsDom.all._

/**
  * A simple UI Label component that was injected with an Behaviour
  * To facilitate interoperability
  *
  * @param text
  */
class Label(text: Behaviour[String]) {

  val initialValue = text.sampleNoTrans()
  // UI - using Scalatags
  val element = p(style := "margin: 0")(initialValue)
  var domElement = element.render

  // Logic
  var listener = text
    .changes()
    .listen(x => {
      val newLast = p(style := "margin: 0")(x).render
      domElement.parentElement.replaceChild(newLast, domElement)
      domElement = newLast
    })
}
