package mw.panel

import mw.panel.Bloc.State.{Free, Occupied}
import mw.panel.Route.State.Active
import mw.panel.Signal.State
import mw.panel.Signal.State.Stop
import mw.react.{EventSource, Reactive}

trait Signal {
	val state: Reactive[State]
	override def toString = s"Signal(state=$state)"
}
object Signal {
	def apply(bloc: Bloc, side: Side): Signal = new Signal {
		val state = EventSource[State]
		state() = Stop
		val pairs = for {
			route <- bloc.route(side)
			state <- route.state
		} yield {
			route -> state
		}
		private val _state = pairs.scan(Map.empty[Route, Route.State])(_ + _).map { map =>
			val routes = map.collect {
				case (route, Active) => route
			}
			val route = if (routes.size == 1) Some(routes.head)
			else None
			route.map { route =>
				val (nextBloc, nextSide) = route.other(bloc)
				val nextSignal = nextBloc.signal(nextSide.other)
				for {
					nextBlocState <- nextBloc.state
					nextSignalState <- nextSignal.state
				} yield {
					nextBlocState match {
						case Occupied => Stop
						case Free => nextSignalState.previous
					}
				}
			}.getOrElse(Reactive.singleton(Stop))
		}.switch
		_state.onEvent { event =>
			state.trigger(event)
		}
	}
	sealed trait State {
		val value: Int
		def previous: State
	}
	object State {
		case object Stop extends State {
			val value = 1
			def previous: Warning.type = Warning
		}
		case object Go extends State {
			val value = 0
			def previous: Go.type = Go
		}
		case object Warning extends State {
			val value = 2
			def previous: Go.type = Go
		}
	}
}
