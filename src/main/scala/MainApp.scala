import org.scalajs.dom
import dom.{document, window}

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.DomApi
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.domtypes.generic.builders.Tag
import com.raquo.laminar.builders.HtmlTag
import com.raquo.laminar.nodes.ParentNode

object MainApp {

  abstract class Component[+Ref <: dom.html.Element](
      tag: HtmlTag[Ref]
  ) extends ReactiveHtmlElement[Ref](tag) {

    def view: ReactiveHtmlElement[Ref]

  }

  abstract class ComponentTag[C <: Component[dom.html.Element]](
      createComponent: () => C
  ) extends HtmlTag[dom.html.Element]("_boo_", false) {

    def apply(modifiers1: Modifier[C]*)(
        modifiers2: Modifier[ReactiveHtmlElement[dom.html.Element]]*
    ): ReactiveHtmlElement[dom.html.Element] = {
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
          ReactiveElement.bindFn(element, value) { value =>
            fun(value)(element)
          }
      }

  }

// Usage

  class BooInput extends Component(div) {
    case class State(text: String) {
      def update(c: Command) = c match {
        case Clear  => copy(text = "")
        case Set(s) => copy(text = s)
        case _      => this
      }
    }

    sealed trait Command
    case object Clear extends Command
    case class Set(s: String) extends Command
    case object DoSomething extends Command
    val state: Var[State] = Var(State(""))
    val stateObserver =
      Observer[Command](c => state.update(_.update(c)))

    val view = div(
      span(color := "", child.text <-- state.signal.map(s => {
        s.text
      })),
      form(
        input(
          typ := "text",
          inContext(thisNode =>
            onInput
              .mapTo(thisNode.ref.value)
              .map(Set(_)) --> stateObserver
          )
        )
      )
    )

  }

  object BooInputTag extends ComponentTag(() => new BooInput)

  object BooInput {

    val text =
      ComponentAttribute[String, BooInput, dom.html.Element](value =>
        component => component.state.update(_.copy(value))
      )

  }

  // Mounting

  def main(args: Array[String]): Unit = {
    println("Starting 'laminar-component'...")

    val bus = new EventBus[String]

    val d = div(
      BooInputTag(BooInput.text := "hi there 2", BooInput.text <-- bus.events)(
        width := "200px"
      ),
      // span(child.text <-- bus.events.collect {
      //   case BooInput.DoSomething => "boo"
      //   case _                    => ""
      // }),
      input(
        typ := "text",
        inContext(thisNode =>
          onInput
            .mapTo(thisNode.ref.value) --> bus.writer
        )
      )
    )

    // implicit val owner = new Owner {}
    // val subscription = bus.events
    //   .filter {
    //     case MainApp.BooInput.DoSomething => true
    //     case _                            => false
    //   }
    //   .foreach(doSomething => println("doing something"))

    render(dom.document.body, d)

    val p = document.createElement("p")
    val text = document.createTextNode("Hello!")
    p.appendChild(text)
    document.body.appendChild(p)
  }

}
