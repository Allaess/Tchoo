package mw.tchoo

import mw.react.Reactive

trait Sensor {
	val state: Reactive[Boolean]
	val loco: Reactive[Option[Loco]]
	val orientation: Reactive[Option[Boolean]]
	override def toString = s"Sensor(state=$state,loco=$loco,orientation=$orientation)"
}
object Sensor {
	private var sensors = Map.empty[(Ecos, Int, Int), Sensor]
	def apply(ecos: Ecos, oid: Int, port: Int): Sensor = sensors.get(ecos, oid, port) match {
		case Some(sensor) => sensor
		case None =>
			val sensor = new Sensor {
				val module = Module(ecos, oid)
				override val state = for (state <- module.state) yield {
					(state & port) != 0
				}
				override val loco = for {
					map <- module.railcom
					(address, _) <- map.get(port)
				} yield {
					if (address == 0) None
					else Some(Loco(ecos, address))
				}
				override val orientation = for {
					map <- module.railcom
					(address, orientation) <- map.get(port)
				} yield {
					if (address == 0) None
					else Some(orientation)
				}
			}
			sensors += (ecos, oid, port) -> sensor
			sensor
	}
}
