package mw.tchoo

import mw.react.Reactive

trait Accessory {
	val oid: Reactive[Int]
	val state: Reactive[Int]
	val valid: Reactive[Boolean]
	val switching: Reactive[Boolean]
	override def toString = s"Accessory(state=$state,valid=$valid,switching=$switching)"
}
object Accessory {
	val managerRequest = Request("queryObjects(11,name1,name2,name3,objectclass)")
	private var accessories = Map.empty[(Ecos, (String, String, String)), Accessory]
	def apply(ecos: Ecos, names: (String, String, String)): Accessory = accessories.get(ecos, names) match {
		case Some(accessory) => accessory
		case None =>
			val accessory = new Accessory {
				val oid = for {
					entry@Entry(oid, _) <- ecos.entries(Request(s"queryObjects(11,name1,name2,name3,objectclass)"))
					name1 <- entry.get[String]("name1") if name1 == names._1
					name2 <- entry.get[String]("name2") if name2 == names._2
					name3 <- entry.get[String]("name3") if name3 == names._3
					"accessory" <- entry.get[String]("objectclass")
				} yield {
					oid
				}
				val request = for (oid <- oid) yield {
					Request(s"get($oid,state,position,switching)")
				}
				val state = for {
					request <- request
					value <- ecos.values[Int](request, "state")
				} yield {
					value
				}
				val valid = for {
					request <- request
					value <- ecos.values[String](request, "position")
				} yield {
					value match {
						case "ok" => true
						case "wrong" => false
					}
				}
				val switching = for {
					request <- request
					value <- ecos.values[Int](request, "switching")
				} yield {
					value != 0
				}
			}
			accessories += (ecos, names) -> accessory
			accessory
	}
}
