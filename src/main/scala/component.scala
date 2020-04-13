package laminarcomponent

import org.scalajs.dom
import dom.{document, window}

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.DomApi
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.domtypes.generic.builders.Tag
import com.raquo.laminar.builders.HtmlTag
import com.raquo.laminar.nodes.ParentNode

abstract class Component[+Ref <: dom.html.Element](
    tag: HtmlTag[Ref]
) extends ReactiveHtmlElement[Ref](tag)
    with Owner {

  def view: ReactiveHtmlElement[Ref]

}

object Component {
  def sendContext[Parent <: Component[_], Child <: Component[_], State](
      parent: Parent
  )(parentState: Signal[State])(
      updateChild: State => Unit
  ) = {
    parentState
      .foreach(s => updateChild(s))(parent)
  }
}

abstract class ComponentTag[C <: Component[dom.html.Element]](
    createComponent: () => C
) extends HtmlTag[dom.html.Element]("_boo_", false) {

  def apply(modifiers1: Modifier[C]*)(
      modifiers2: Modifier[ReactiveHtmlElement[dom.html.Element]]*
  ): C = {
    val element = build()
    modifiers1.foreach(modifier => modifier(element))
    modifiers2.foreach(modifier => modifier(element))
    element
  }
  override def build = {
    val elem = createComponent()
    elem.amend(elem.view)
    elem
  }
}

case class ComponentAttribute[V, C <: Component[Ref], +Ref <: dom.html.Element](
    fun: V => C => Unit
) {
  def :=(value: V) = new Modifier[C] {
    override def apply(element: C): Unit = fun(value)(element)
  }
  def <--(value: Observable[V]) =
    new Modifier[C] {
      override def apply(element: C): Unit =
        ReactiveElement.bindFn(element, value) { value => fun(value)(element) }
    }

}

case class ComponentEvent[V, C <: Component[Ref], +Ref <: dom.html.Element](
    fun: C => Observable[V]
) {

  def -->(observer: Observer[V]) =
    new Modifier[C] {
      override def apply(element: C): Unit =
        ReactiveElement.bindObserver(element, fun(element))(observer)
    }

}
