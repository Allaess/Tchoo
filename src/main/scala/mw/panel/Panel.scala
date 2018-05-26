package mw.panel

import mw.tchoo
import mw.hetero.{::, CSV, HNil}

case class Panel(ecos: tchoo.Ecos, name: String) {
	private var colors = Map.empty[Range, Color]
	// Leds
	for (names :: pin :: HNil <-
		     CSV.read[(String, String, String) :: Int :: HNil](s"$name/leds.csv")) {
		val accessory = tchoo.Accessory(ecos, names)
		val led = Led(pin, accessory)
		for (state <- led.state) {
			this (led) = state
		}
	}
	// Blocs
	for (name :: oid :: port :: from :: to :: HNil <-
		     CSV.read[String :: Int :: Int :: Int :: Int :: HNil](s"$name/blocs.csv")) {
		val range = Range(from, to)
		val sensor = tchoo.Sensor(ecos, oid, port)
		val bloc = Bloc(name, range, sensor)
		for (state <- bloc.state) {
			this (range) = state.color
		}
	}
	// Routes
	for (entry :: entrySide :: exit :: exitSide :: names :: ranges :: HNil <-
		     CSV.read[String :: String :: String :: String :: (String, String, String) :: List[Range] :: HNil]
			     (s"$name/routes.csv")) {
		val ecosRoute = tchoo.Route(ecos, names)
		val route = Route(ranges, ecosRoute)
		for {
			state <- route.state
			range <- ranges
		} {
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
