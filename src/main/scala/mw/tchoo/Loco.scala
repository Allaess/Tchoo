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
				val oid = for {
					entries <- ecos.reply(managerRequest)
					entry <- entries
					"loco" <- entry.get[String]("objectclass")
					addr <- entry.get[Int]("addr") if addr == address
				} yield {
					entry.oid
				}
				private def request(oid: Int) = Request(s"get($oid,name,speed,dir)")
				val name = {
					for (oid <- oid) yield {
						for (value <- ecos.value[String](request(oid), "name")) yield {
							value
						}
					}
				}.switch
				val speed = {
					for (oid <- oid) yield {
						for (value <- ecos.value[Int](request(oid), "speed")) yield {
							value
						}
					}
				}.switch
				val direction = {
					for (oid <- oid) yield {
						for (value <- ecos.value[Int](request(oid), "dir")) yield {
							value != 0
						}
					}
				}.switch
			}
			locos += (ecos, address) -> loco
			loco
	}
}
