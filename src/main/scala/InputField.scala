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

case class FormContext(
    disabled: Boolean,
    error: Boolean,
    required: Boolean
)
object FormContext {
  val default = FormContext(false, false, false)
}

class InputField extends FormComponent {
  import InputField.{State, Comm, Set}

  val state: Var[State] = Var(State.default)

  val formContextObserver =
    Observer[FormContext](fc => {
      state.update(_.copy(formContext = fc))
    })

  val commandObserver =
    Observer[Comm](c => state.update(_.update(c)))

  val view =
    input(
      typ <-- state.signal.map(_.typ),
      placeholder <-- state.signal.map(_.placeholder.getOrElse("")),
      value <-- state.signal.map(_.value),
      disabled <-- state.signal
        .map(_.formContext.disabled),
      required <-- state.signal.map(_.formContext.required),
      inContext(thisNode =>
        onInput
          .mapTo(thisNode.ref.value)
          .map(Set(_)) --> commandObserver
      )
    )

}

object InputFieldTag extends ComponentTag(() => new InputField)

object InputField {

  private[InputField] sealed trait Comm
  case object Clear extends Comm
  case class Set(s: String) extends Comm

  case class State(
      placeholder: Option[String],
      formContext: FormContext,
      typ: String,
      value: String
  ) {
    def update(c: Comm) = c match {
      case Set(s) => copy(value = s)
      case Clear  => copy(value = "")
    }
  }
  object State {
    val default = State(None, FormContext.default, "text", "")
  }

  def setter[A](updateF: (State, A) => State) =
    ComponentAttribute.setter[A, InputField, State](_.state)(
      updateF
    )

  val value = setter[String] { case (state, v1) => state.copy(value = v1) }
  val typ = setter[String] { case (state, v)    => state.copy(typ = v) }
  val placeholder = setter[Option[String]] {
    case (state, v) => state.copy(placeholder = v)
  }
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

  val state =
    ComponentEvent[State, InputField](
      _.state.signal
    )

}
