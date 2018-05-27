package mw.panel

import mw.panel.Signal.State
import mw.react.Reactive

trait Signal {
	val state: Reactive[State]
	override def toString = s"Signal(state=$state)"
}
object Signal {
	sealed trait State {
		def previous: State
	}
	object State {
		case object Stop extends State {
			def previous = Warning
		}
		case object Go extends State {
			def previous = Go
		}
		case object Warning extends State {
			def previous = Go
		}
	}
}
