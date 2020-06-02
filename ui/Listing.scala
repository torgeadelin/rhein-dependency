package rhein.ui
import rhein._
import scala.scalajs.js
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import scala.collection.{Iterable => Iter}

/**import
  * A simple UI Listing component that was can be injected
  * with a behaviour that holds a list of items and a function describing
  * how to render each element in the list
  * @param value
  * @param f maps an element from the list to a dom element
  */

class Listing[T](
    value: Behaviour[List[T]],
    f: (T, Int) => scalatags.JsDom.Modifier
) {

  // Injection
  val initialValue: List[T] = List()
  val element = span(
    for ((elem, index) <- initialValue.toSeq.zipWithIndex) yield f(elem, index)
  )

  var domElement = element.render

  // Force re-render every time a new value is received
  var listener = value
    .changes()
    .listen((newVal: List[T]) => {
      val newLast =
        span(
          for ((elem, index) <- newVal.toSeq.zipWithIndex) yield f(elem, index)
        ).render
      domElement.parentElement.replaceChild(newLast, domElement)
      domElement = newLast
    })
}
