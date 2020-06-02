package rhein.ui
import rhein._
import scala.scalajs.js
import scalatags.JsDom.all._

/**
  * Simple class representing the initial value emited
  * by the click event
  */
class EmptyMessage extends Message

/**
  * A simple UI Button component that was injected with an Event Stream
  * To facilitate interoperability
  *
  * @param text
  * @param label used to provide an css ID
  */
class Button(
    val text: String,
    val label: String
) {

  // Injection
  var valueToEmit: Message = new EmptyMessage()
  var eventClicked: Event[Message] = new Event()
  var eventClickedSink: EventSink[Message] = new EventSink()
  eventClicked = eventClickedSink

  // UI - using Scalatags
  val domElement = div(id := label, cls := "btn btn-primary", onclick := { () =>
    {
      eventClickedSink.send(valueToEmit)
    }
  })(text)

  /**
    * helper method to inject a new event stream
    * inside this component and provide a specific
    * value to emit
    *
    * @param event
    * @param newValueToEmit
    */
  def attachEvent(event: Event[_], newValueToEmit: Message) {
    valueToEmit = newValueToEmit
    eventClickedSink = event.asInstanceOf[EventSink[Message]]
  }

  /**
    * Auxiliary constructor
    *
    * @param text
    * @return
    */
  def this(text: String) {
    this(text, "")
  }

}
