package mw.tchoo

import mw.react.Reactive

trait Loco {
	val name: Reactive[String]
	val speed: Reactive[Int]
	val direction: Reactive[Boolean]
	override def toString = s"Loco(name=$name,speed=$speed,direction=$direction)"
}
object Loco {
	private var locos = Map.empty[(Ecos, Int), Loco]
	def apply(ecos: Ecos, ADDRESS: Int): Loco = locos.get(ecos, ADDRESS) match {
		case Some(loco) => loco
		case None =>
			val loco = new Loco {
				val oid = for {
					entry@Entry(oid, _) <- ecos.entries(Request(s"queryObjects(10,addr,objectclass)"))
					"loco" <- entry.get[String]("objectclass")
					ADDRESS <- entry.get[Int]("addr")
				} yield {
					oid
				}
				val request = for (oid <- oid) yield {
					Request(s"get($oid,name,speed,dir)")
				}
				val name = for {
					request <- request
					value <- ecos.values[String](request, "name")
				} yield {
					value
				}
				val speed = for {
					request <- request
					value <- ecos.values[Int](request, "speed")
				} yield {
					value
				}
				val direction = for {
					request <- request
					value <- ecos.values[Int](request, "dir")
				} yield {
					value != 0
				}
			}
			locos += (ecos, ADDRESS) -> loco
			loco
	}
}
