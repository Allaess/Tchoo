package mw.panel

import mw.hetero.{Decode, DecodeException}
import mw.panel.Bloc.State
import mw.panel.Bloc.State.{Free, Occupied}
import mw.panel.Route.State.Active
import mw.panel.Signal.State.Stop
import mw.react.{EventSource, Reactive}
import mw.tchoo.Sensor

trait Bloc {
	val range: Range
	val state: Reactive[State]
	protected val entrySignal: Signal
	protected val exitSignal: Signal
	private val entryRoutes = EventSource[Route]
	private val exitRoutes = EventSource[Route]
	protected def route(side: Side): Reactive[Route] = side match {
		case Entry => entryRoutes
		case Exit => exitRoutes
	}
	def routes(side: Side): Addable = { route =>
		side match {
			case Entry => entryRoutes() = route
			case Exit => exitRoutes() = route
		}
	}
	def signal(side: Side): Signal = side match {
		case Entry => entrySignal
		case Exit => exitSignal
	}
	override def toString = s"Bloc(range=$range,state=$state)"
	trait Addable {
		def +=(route: Route): Unit
	}
}
object Bloc {
	implicit val decode: Decode[Bloc] = new Decode[Bloc] {
		val minSize = 1
		def apply(data: List[String]) = data match {
			case head :: tail => (Bloc(head), tail)
			case Nil => throw DecodeException("No data left for Bloc")
		}
	}
	private var blocs = Map.empty[String, Bloc]
	def apply(name: String): Bloc = blocs(name)
	def apply(name: String, _range: Range, sensor: Sensor): Bloc = blocs.get(name) match {
		case Some(bloc) => bloc
		case None =>
			val bloc = new Bloc {
				val range = _range
				val state = for (state <- sensor.state) yield {
					State(state)
				}
				val entrySignal: Signal = new Signal {
					val state = nextSignalState(Entry)
				}
				val exitSignal: Signal = new Signal {
					val state = nextSignalState(Exit)
				}
				private def route2state(side: Side): Reactive[(Route, Route.State)] = for {
					route <- route(side)
					state <- route.state
				} yield {
					route -> state
				}
				private def activeRoute(side: Side): Reactive[Option[Route]] = {
					route2state(side).scan(Map.empty[Route, Route.State])(_ + _).map { map =>
						val coll = map.collect {
							case (route, Active) => route
						}
						if (coll.size == 1) Some(coll.head)
						else None
					}
				}
				private def nextSignalState(side: Side): Reactive[Signal.State] = {
					activeRoute(side).map { option =>
						option.map { route =>
							val (nextBloc, nextSide) = route.other(this)
							val nextSignal = nextBloc.signal(nextSide.other)
							for {
								blocState <- nextBloc.state
								signalState <- nextSignal.state
							} yield {
								blocState match {
									case Occupied => Stop
									case Free => signalState.previous
								}
							}
						}.getOrElse(Reactive.singleton(Stop))
					}.switch
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
}
