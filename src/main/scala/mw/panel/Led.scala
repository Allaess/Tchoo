package mw.panel

import mw.react.Reactive
import mw.tchoo.Accessory

trait Led {
	val pin: Int
	val state: Reactive[Boolean]
	override def toString = s"Led(pin=$pin,state=$state)"
}
object Led {
	def apply(_pin: Int, accessory: Accessory): Led = new Led {
		val pin = _pin
		val state = for (state <- accessory.state) yield {
			state == 0
		}
	}
}
