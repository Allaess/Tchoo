package mw.panel

import mw.panel.Bloc.{Side, State}
import mw.panel.Bloc.Side.{Entry, Exit}
import mw.react.{EventSource, Reactive}
import mw.tchoo.Sensor

trait Bloc {
	val range: Range
	val state: Reactive[State]
	private val entryRoutes = EventSource[Route]
	private val exitRoutes = EventSource[Route]
	def routes(side: Side): Addable = { route =>
		side match {
			case Entry => entryRoutes() = route
			case Exit => exitRoutes() = route
		}
	}
	override def toString = s"Bloc(range=$range,state=$state)"
	trait Addable {
		def +=(route: Route): Unit
	}
}
object Bloc {
	private var blocs = Map.empty[String, Bloc]
	def apply(name: String, _range: Range, sensor: Sensor): Bloc = blocs.get(name) match {
		case Some(bloc) => bloc
		case None =>
			val bloc = new Bloc {
				val range = _range
				val state = for (state <- sensor.state) yield {
					State(state)
				}
			}
			blocs += name -> bloc
			bloc
	}
	sealed trait State {
		val color: Color
	}
	object State {
		def apply(flag: Boolean): State =
			if (flag) Occupied
			else Free
		case object Occupied extends State {
			val color = Color(31, 0, 0)
		}
		case object Free extends State {
			val color = Color(0, 0, 0)
		}
	}
	sealed trait Side
	object Side {
		def apply(text: String): Side = text match {
			case "entry" => Entry
			case "exit" => Exit
		}
		case object Entry extends Side
		case object Exit extends Side
	}
}
