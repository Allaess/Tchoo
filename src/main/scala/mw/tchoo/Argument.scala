package mw.tchoo

case class Argument(name: String, values: List[String]) {
	override def toString: String = values match {
		case Nil => name
		case _ => s"$name[${values.mkString(",")}]"
	}
}
