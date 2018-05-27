package mw.panel

import mw.panel.Signal.State
import mw.react.Reactive

trait Signal {
	val state: Reactive[State]
	override def toString = s"Signal(state=$state)"
}
object Signal {
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
