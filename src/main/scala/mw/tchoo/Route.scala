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
				val request = for {
					entry@Entry(oid, _) <- ecos.entries(Request(s"queryObjects(11,name1,name2,name3,objectclass)"))
					name1 <- entry.get[String]("name1") if name1 == names._1
					name2 <- entry.get[String]("name2") if name2 == names._2
					name3 <- entry.get[String]("name3") if name3 == names._3
					"route" <- entry.get[String]("objectclass")
				} yield {
					Request(s"get($oid,state,switching)")
				}
				val state = for {
					request <- request
					value <- ecos.values[Int](request, "state")
				} yield {
					value
				}
				val switching = for {
					request <- request
					value <- ecos.values[Int](request, "switching")
				} yield {
					value != 0
				}
			}
			routes += (ecos, names) -> route
			route
	}
}
