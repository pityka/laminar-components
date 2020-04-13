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

    val d = div(
      BooInputTag(
        BooInput.text := "hi there 2",
        BooInput.text <-- bus.events,
        BooInput.state --> bus2.writer,
        BooInput.children := BooInputTag(BooInput.text := "hi there")()
      )(
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
      ),
      span(child.text <-- bus2.events.map(_.toString))
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
