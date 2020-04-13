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

class TextField extends FormComponent {
  import TextField.{State}

  val state: Var[State] = Var(State.default)

  val formContextObserver =
    Observer[FormContext](fc => state.update(_.copy(formContext = fc)))

  val inputFieldStateObserver =
    Observer[InputField.State](st => state.update(_.copy(value = st.value)))

  val error =
    state.signal.changes.map(s => (s, s.validate(s.value)))

  val helperText = error.map {
    case (state, error) => error.getOrElse(state.helperText)
  }

  val view =
    div(
      InputLabelTag.component(
        InputLabel.text <-- state.signal.map(_.label),
        InputLabel.formContext <-- state.signal.map(_.formContext),
        ComponentAttribute.child := InputFieldTag.component(
          InputField.placeholder <-- state.signal.map(_.placeholder),
          InputField.value <-- state.signal.map(_.value),
          InputField.state --> inputFieldStateObserver,
          InputField.formContext <-- state.signal.map(_.formContext)
        )
      ),
      FormHelperTextTag.component(
        FormHelperText.text <-- helperText,
        FormHelperText.formContext <-- state.signal.map(_.formContext)
      )
    )

  error
    .map(_._2.isDefined)
    .foreach { b =>
      state.update(s => s.copy(formContext = s.formContext.copy(error = b)))
    }(this)

}

object TextFieldTag extends ComponentTag(() => new TextField)

object TextField {

  private[TextField] case class State(
      formContext: FormContext,
      label: String,
      value: String,
      placeholder: Option[String],
      typ: String,
      helperText: String,
      validate: String => Option[String]
  )
  object State {
    val default =
      State(FormContext.default, "", "", None, "text", "", _ => None)
  }

  def setter[A](updateF: (State, A) => State) =
    ComponentAttribute.setter[A, TextField, State](_.state)(
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
  val value = setter[String] { case (state, v1) => state.copy(value = v1) }
  val typ = setter[String] { case (state, v)    => state.copy(typ = v) }
  val label = setter[String] { case (state, v)  => state.copy(label = v) }
  val helperText = setter[String] {
    case (state, v) => state.copy(helperText = v)
  }
  val placeholder = setter[Option[String]] {
    case (state, v) => state.copy(placeholder = v)
  }
  val validation = setter[String => Option[String]] {
    case (state, v) => state.copy(validate = v)
  }
  val state =
    ComponentEvent[State, TextField](
      _.state.signal
    )

}
