package mw.panel

import mw.hetero.{::, CSV, HNil}
import mw.tchoo.{Accessory, Ecos, Sensor}

case class Panel(ecos: Ecos, name: String) {
	private var colors = Map.empty[Range, Color]
	// Leds
	for (names :: pin :: HNil <-
		     CSV.read[(String, String, String) :: Int :: HNil](s"$name/leds.csv")) {
		val accessory = Accessory(ecos, names)
		val led = Led(pin, accessory)
		led.state.onData { state =>
			this (led) = state
		}
	}
	// Blocs
	for (oid :: port :: from :: to :: HNil <-
		     CSV.read[Int :: Int :: Int :: Int :: HNil](s"$name/blocs.csv")) {
		val range = Range(from, to)
		val sensor = Sensor(ecos, oid, port)
		val bloc = Bloc(range, sensor)
		bloc.state.onData { state =>
			this (range) = state.color
		}
	}
	def update(led: Led, state: Boolean): Unit =
		if (state) println(s"s${led.pin}")
		else println(s"r${led.pin}")
	def update(range: Range, color: Color): Unit = {
		if (color == Color.empty) colors -= range
		else colors += range -> color
		for (index <- range) {
			val cs = for ((r, c) <- colors if r.contains(index)) yield {
				c
			}
			val c = (Color.empty /: cs) (_ | _)
			println(s"c$index=${color.red},${color.green},${color.blue}")
		}
	}
}
