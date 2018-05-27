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
	for (entryBloc :: entrySide :: exitBloc :: exitSide :: names :: ranges :: HNil <-
		     CSV.read[Bloc :: Side :: Bloc :: Side :: (String, String, String) :: List[Range] :: HNil]
			     (s"$name/routes.csv")) {
		val ecosRoute = tchoo.Route(ecos, names)
		val route = Route(entryBloc, entrySide, exitBloc, exitSide, ecosRoute, ranges)
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
		val list = for (index <- range) yield {
			val cs = for ((r, c) <- colors if r.contains(index)) yield {
				c
			}
			index -> (Color.empty /: cs) (_ | _)
		}
		paint(list)
	}
	private def paint(list: Iterable[(Int, Color)]): Unit = if (list.nonEmpty) {
		val (first, color) = list.head
		val (taken, dropped) = list.span(_._2 == color)
		val last = first + taken.size
		println(s"c$first-$last=$color")
		paint(dropped)
	}
}
