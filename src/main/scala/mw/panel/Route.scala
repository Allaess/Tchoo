package mw.panel

import mw.tchoo
import mw.panel.Route.State
import mw.panel.Route.State._
import mw.react.Reactive

trait Route {
	val ranges: List[Range]
	val state: Reactive[State]
	def other(bloc: Bloc): (Bloc, Side)
	override def toString = s"Route(state=$state)"
}
object Route {
	def apply(entryBloc: Bloc, entrySide: Side, exitBloc: Bloc, exitSide: Side,
	          ecosRoute: tchoo.Route, _ranges: List[Range]): Route = new Route {
		val ranges = _ranges
		val state = for {
			state <- ecosRoute.state
			switching <- ecosRoute.switching
		} yield {
			if (switching) Switching
			else if (state != 0) Active
			else Inactive
		}
		entryBloc.routes(entrySide) += this
		exitBloc.routes(exitSide) += this
		def other(bloc: Bloc) = {
			if (bloc == entryBloc) (exitBloc, exitSide)
			else if (bloc == exitBloc) (entryBloc, entrySide)
			else throw new Exception("Unknown bloc")
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
