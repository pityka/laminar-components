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

class BooInput extends Component {
  case class State(
      text: String,
      desc: List[ReactiveHtmlElement[dom.html.Element]]
  ) {
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
  val state: Var[State] = Var(State("", Nil))
  val stateObserver =
    Observer[Command](c => state.update(_.update(c)))

  val view = div(
    span(color := "", child.text <-- state.signal.map(_.text)),
    form(
      input(
        typ := "text",
        inContext(thisNode =>
          onInput
            .mapTo(thisNode.ref.value)
            .map(Set(_)) --> stateObserver
        )
      ),
      children <-- state.signal.map(_.desc.toList)
    )
  )

}

object BooInputTag extends ComponentTag(() => new BooInput)

object BooInput {

  def children =
    ComponentAttribute[BooInput, BooInput] { value => component =>
      component.state.update(_.copy(desc = List(value)))
    }

  val text =
    ComponentAttribute[String, BooInput](value =>
      component => component.state.update(_.copy(value))
    )

  val state =
    ComponentEvent[BooInput#State, BooInput](_.state.signal)

}
