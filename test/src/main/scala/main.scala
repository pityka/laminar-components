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

object MainApp {

  def main(args: Array[String]): Unit = {
    println("Starting 'laminar-component'...")

    val bus = new EventBus[String]
    val bus2 = new EventBus[BooInput#State]

    // val d = div(
    //   BooInputTag.comp(
    //     BooInput.text := "hi there 2",
    //     BooInput.text <-- bus.events,
    //     BooInput.state --> bus2.writer,
    //     BooInput.children := BooInputTag.comp(BooInput.text := "hi there")()
    //   )(
    //     width := "200px"
    //   ),
    //   // span(child.text <-- bus.events.collect {
    //   //   case BooInput.DoSomething => "boo"
    //   //   case _                    => ""
    //   // }),
    //   input(
    //     typ := "text",
    //     inContext(thisNode =>
    //       onInput
    //         .mapTo(thisNode.ref.value) --> bus.writer
    //     )
    //   ),
    //   span(child.text <-- bus2.events.map(_.toString))
    // )

    var clicked = Var(false)

    val unitObserver = Observer[Unit] { _ =>
      if (clicked.now == true) clicked.update(_ => false)
      else clicked.update(_ => true)
    }

    val d = div(
      form(
        TextFieldTag.component(
          TextField.label := "label",
          TextField.state.map(_.toString) --> bus.writer,
          TextField.helperText := "whatever",
          TextField.disabled <-- clicked.signal,
          TextField.validation := (s =>
            if (s.startsWith("A")) Some("AA") else None
          )
        )
      ),
      div(child.text <-- bus.events),
      div(child.text <-- clicked.signal.map(_.toString)),
      button(
        "click",
        typ := "button",
        inContext(thisNode =>
          onClick
            .mapTo(thisNode.ref.value)
            .map(_ => ()) --> unitObserver
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
