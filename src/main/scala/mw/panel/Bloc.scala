package mw.panel

import mw.panel.Bloc.State
import mw.react.Reactive
import mw.tchoo.Sensor

trait Bloc {
	val range: Range
	val state: Reactive[State]
	override def toString = s"Bloc(range=$range,state=$state)"
}
object Bloc {
	def apply(_range: Range, sensor: Sensor): Bloc = new Bloc {
		override val range = _range
		override val state = for (state <- sensor.state) yield {
			State(state)
		}
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
