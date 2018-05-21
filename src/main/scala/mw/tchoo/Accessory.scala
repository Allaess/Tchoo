package mw.tchoo

import mw.react.Reactive

trait Accessory {
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
				val request: Reactive[Request] = for {
					entries <- ecos.entries(managerRequest, "name1", "name2", "name3", "objectclass")
					entry <- entries
					"accessory" <- entry.get[String]("objectclass")
					name1 <- entry.get[String]("name1") if name1 == names._1
					name2 <- entry.get[String]("name2") if name2 == names._2
					name3 <- entry.get[String]("name3") if name3 == names._3
				} yield {
					Request(s"get(${entry.oid},state,position,switching)")
				}
				val state: Reactive[Int] = for {
					request <- request
					entries <- ecos.entries(request, "state")
					entry <- entries
					value <- entry.get[Int]("state")
				} yield {
					value
				}
				val valid: Reactive[Boolean] = for {
					request <- request
					entries <- ecos.entries(request, "position")
					entry <- entries
					value <- entry.get[String]("position")
				} yield {
					value match {
						case "ok" => true
						case "wrong" => false
					}
				}
				val switching: Reactive[Boolean] = for {
					request <- request
					entries <- ecos.entries(request, "switching")
					entry <- entries
					value <- entry.get[Int]("switching")
				} yield {
					value != 0
				}
			}
			accessories += (ecos, names) -> accessory
			accessory
	}
}
