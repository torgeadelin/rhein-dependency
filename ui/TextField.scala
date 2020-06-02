package rhein.ui
import rhein._
import scala.scalajs.js
import scalatags.JsDom.all._
import org.scalajs.dom.{Event => DomEvent}

/**
  * Text field component corresponding to a 
  * <input type="text"/> HTML component that has an
  * event injected
  *
  * @param sText
  * @param initialValue
  */
class TextField(
    var sText: Event[String],
    var initialValue: String,
) {

  // Injection
  final val userChangesSink: EventSink[String] = new EventSink()
  var userChanges: Event[String] = new Event()

  userChanges = userChangesSink
  val merged = Event.merge(userChangesSink, sText)
  var text: Behaviour[String] = merged.hold(initialValue)

  sText.listen((newVal) => {
    domElement.value = newVal
  })

  // <input> element using scalatags
  val element =
    input(`type` := "text", 
      cls := "form-control", 
      value := text.sampleNoTrans, 
      style := "max-width: 250px")

    // property used to render this component to the dom
  var domElement = element.render

  /**
    * attaching a DOM listener where we emit events
    * whenever the text inside the input changes
    */
  domElement.oninput = (event: DomEvent) => {
    val newVal = domElement.value.toString
    userChangesSink.send(newVal)
  }

  /**
    * Auxiliary constructor
    *
    * @param initialValue
    * @return
    */
  def this(initialValue: String) {
    this(new Event[String], initialValue)
  }
}
