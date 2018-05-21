package mw.tchoo

case class Request(cmd: String, oid: Int, args: List[Argument]) {
	override def toString: String = args match {
		case Nil => s"$cmd($oid)"
		case _ => s"$cmd($oid,${args.mkString(",")})"
	}
}
object Request {
	def apply(text: String): Request = Ecos.parseRequest(text)
}
