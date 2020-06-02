package rhein.ui
import org.scalajs.dom.html
import org.scalajs.dom.{Element}
import scala.scalajs.js
import scalatags.JsDom.all._

import rhein._

/**
  * Bindings is just a wrapper
  * for implicit functions that convert
  * behaviours into dom elements
  *
  * REF! - Ideas based on:
  * https://www.youtube.com/watch?v=i9mPUU1gu_8
  */
object Bindings {

  /**
    * Converts Behaviour that is used in the
    * context of a dom element in scalatags to
    * an actual dom element (Modifier)
    *
    * @return Modifier
    */
  implicit def BehaviourToDom[T](r: Behaviour[T])(
      implicit f: T => Modifier
  ): Modifier = {
    var initialValue = r.sampleNoTrans()

    // UI - using Scalatags
    val element = span(initialValue)
    var domElement = element.render

    // Logic
    var listener = r
      .changes()
      .listen(x => {
        val newLast = span(x).render
        domElement.parentElement.replaceChild(newLast, domElement)
        domElement = newLast
      })

    domElement
  }

  /**
    * Converts Behaviour that is used in the
    * context of a attribute value in scalatags to
    * an actual attribute value
    *
    * @return AttrValue
    */
  implicit def BehaviourToAttrValue[T: AttrValue] =
    new AttrValue[Behaviour[T]] {
      def apply(t: Element, a: Attr, r: Behaviour[T]): Unit = {
        r.changes()
          .listen((newVal) => {
            implicitly[AttrValue[T]].apply(t, a, newVal)
          })
      }
    }

  /**
    * Converts Behaviour that is used in the
    * context of a style value in scalatags to
    * an actual style value
    *
    * @return StyleValue
    */
  implicit def BehaviourToStyleValue[T: StyleValue] =
    new StyleValue[Behaviour[T]] {
      def apply(t: Element, s: Style, r: Behaviour[T]): Unit = {
        r.changes()
          .listen((newVal) => {
            implicitly[StyleValue[T]].apply(t, s, r.sampleNoTrans())
          })
      }
    }
}
