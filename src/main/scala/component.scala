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

abstract class Component
    extends ReactiveHtmlElement[dom.html.Element](span)
    with Owner {

  lazy val desc: Var[List[ReactiveHtmlElement[dom.html.Element]]] = Var(Nil)

  def view: ReactiveHtmlElement[dom.html.Element]

}

abstract class ComponentTag[C <: Component](
    createComponent: () => C
) extends HtmlTag[dom.html.Element]("_boo_", false) {

  def component(modifiers1: Modifier[C]*): C = {
    val element = build()
    modifiers1.foreach(modifier => modifier(element))
    element
  }
  override def build = {
    val elem = createComponent()
    elem.amend(elem.view)
    elem
  }
}

case class ComponentAttribute[V, C <: Component](
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

object ComponentAttribute {
  def children[C <: Component] =
    ComponentAttribute[List[ReactiveHtmlElement[dom.html.Element]], C] {
      value => component => component.desc.update(_ => value)
    }
  def child[C <: Component] =
    ComponentAttribute[ReactiveHtmlElement[dom.html.Element], C] {
      value => component => component.desc.update(_ => List(value))
    }

  def setter[A, C <: Component, State](
      state: C => Var[State]
  )(
      updateF: (State, A) => State
  ) =
    ComponentAttribute[A, C](value =>
      component => state(component).update(s => updateF(s, value))
    )
}

case class ComponentEvent[V, C <: Component](
    fun: C => Observable[V]
) {

  def map[K](fun2: V => K) = ComponentEvent((c: C) => fun(c).map(fun2))

  def -->(observer: Observer[V]) =
    new Modifier[C] {
      override def apply(element: C): Unit =
        ReactiveElement.bindObserver(element, fun(element))(observer)
    }

}
