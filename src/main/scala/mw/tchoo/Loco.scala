package mw.tchoo

import mw.react.Reactive

trait Loco {
	val name: Reactive[String]
	val speed: Reactive[Int]
	val direction: Reactive[Boolean]
	override def toString = s"Loco(name=$name,speed=$speed,direction=$direction)"
}
object Loco {
	val managerRequest = Request("queryObjects(10,addr,objectclass)")
	private var locos = Map.empty[(Ecos, Int), Loco]
	def apply(ecos: Ecos, address: Int): Loco = locos.get(ecos, address) match {
		case Some(loco) => loco
		case None =>
			val loco = new Loco {
				def request = for {
					entries <- ecos.entries(managerRequest, "addr", "objectclass")
					entry <- entries
					"loco" <- entry.get[String]("objectclass")
					addr <- entry.get[Int]("addr") if addr == address
				} yield {
					Request(s"get(${entry.oid},name,speed,dir)")
				}
				val name = for {
					request <- request
					entries <- ecos.entries(request, "name")
					entry <- entries
					value <- entry.get[String]("name")
				} yield {
					value
				}
				val speed = for {
					request <- request
					entries <- ecos.entries(request, "speed")
					entry <- entries
					value <- entry.get[Int]("speed")
				} yield {
					value
				}
				val direction = for {
					request <- request
					entries <- ecos.entries(request, "dir")
					entry <- entries
					value <- entry.get[Int]("dir")
				} yield {
					value != 0
				}
			}
			locos += (ecos, address) -> loco
			loco
	}
}
