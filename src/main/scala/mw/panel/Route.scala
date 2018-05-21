package mw.panel

import mw.tchoo
import mw.panel.Route.State
import mw.panel.Route.State._
import mw.react.Reactive

trait Route {
	val ranges: List[Range]
	val state: Reactive[State]
}
object Route {
	def apply(_ranges: List[Range], ecosRoute: tchoo.Route): Route = new Route {
		val ranges = _ranges
		val state = for {
			state <- ecosRoute.state
			switching <- ecosRoute.switching
		} yield {
			if (switching) Switching
			else if (state != 0) Active
			else Inactive
		}
	}
	sealed trait State {
		val color: Color
	}
	object State {
		case object Active extends State {
			val color = Color(0, 0, 31)
		}
		case object Inactive extends State {
			val color = Color(0, 0, 0)
		}
		case object Switching extends State {
			val color = Color(31, 19, 0)
		}
	}
}
