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

abstract class FormComponent extends Component {
  def formContextObserver: Observer[FormContext]
}

class FormHelperText extends FormComponent {
  import FormHelperText.{State}

  val state: Var[State] = Var(State.default)

  val formContextObserver =
    Observer[FormContext](fc => state.update(_.copy(formContext = fc)))

  val view =
    span(
      disabled <-- state.signal.map(_.formContext.disabled),
      required <-- state.signal.map(_.formContext.required),
      child.text <-- state.signal.map(_.text),
      children <-- desc.signal
    )

}

object FormHelperTextTag extends ComponentTag(() => new FormHelperText)

object FormHelperText {

  private[FormHelperText] case class State(
      formContext: FormContext,
      text: String
  )
  object State {
    val default = State(FormContext.default, "")
  }

  def setter[A](updateF: (State, A) => State) =
    ComponentAttribute.setter[A, FormHelperText, State](_.state)(
      updateF
    )

  val formContext = setter[FormContext] {
    case (state, v) => state.copy(formContext = v)
  }
  val disabled = setter[Boolean] {
    case (state, v) =>
      state.copy(formContext = state.formContext.copy(disabled = v))
  }
  val required = setter[Boolean] {
    case (state, v) =>
      state.copy(formContext = state.formContext.copy(required = v))
  }
  val error = setter[Boolean] {
    case (state, v) =>
      state.copy(formContext = state.formContext.copy(error = v))
  }
  val text = setter[String] {
    case (state, v) =>
      state.copy(text = v)
  }

  val state =
    ComponentEvent[State, FormHelperText](
      _.state.signal
    )

}
