package mw.tchoo

import mw.react.Reactive

trait Route {
	val state: Reactive[Int]
	val switching: Reactive[Boolean]
	override def toString = s"Route(state=$state,switching=$switching)"
}
object Route {
	val managerRequest: Request = Accessory.managerRequest
	private var routes = Map.empty[(Ecos, (String, String, String)), Route]
	def apply(ecos: Ecos, names: (String, String, String)): Route = routes.get(ecos, names) match {
		case Some(route) => route
		case None =>
			val route = new Route {
				val oid = for {
					entries <- ecos.reply(managerRequest)
					entry <- entries
					"route" <- entry.get[String]("objectclass")
					name1 <- entry.get[String]("name1") if name1 == names._1
					name2 <- entry.get[String]("name2") if name2 == names._2
					name3 <- entry.get[String]("name3") if name3 == names._3
				} yield {
					entry.oid
				}
				val state = {
					for (oid <- oid) yield {
						for (value <- ecos.value[Int](oid, "state")) yield {
							value
						}
					}
				}.switch
				val switching = {
					for (oid <- oid) yield {
						for (value <- ecos.value[Int](oid, "switching")) yield {
							value != 0
						}
					}
				}.switch
				for (oid <- oid) {
					ecos.send(s"get($oid,state,switching)")
				}
			}
			routes += (ecos, names) -> route
			route
	}
}
