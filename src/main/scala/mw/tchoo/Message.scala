package mw.tchoo

import mw.hetero.Decode

sealed trait Message {
	val oid: Int
	val entries: List[Entry]
	val errCode: Int
	val errMessage: String
	def get[T](OID: Int, name: String)(implicit decode: Decode[T]): Option[T] = {
		val list = for {
			entry@Entry(OID, _) <- entries
			value <- entry.get[T](name)
		} yield {
			value
		}
		list.headOption
	}
	def get[T](name: String)(implicit decode: Decode[T]): Option[T] = get(oid, name)
}
object Message {
	def apply(start: StartLine, entries: List[Entry], end: EndLine): Message = start match {
		case ReplyLine(request) => Reply(request, entries, end.errCode, end.errMessage)
		case EventLine(oid) => Event(oid, entries, end.errCode, end.errMessage)
	}
	def unapply(message: Message): Option[(Int, List[Entry], Int, String)] =
		Some(message.oid, message.entries, message.errCode, message.errMessage)
}
case class Reply(request: Request, entries: List[Entry], errCode: Int, errMessage: String) extends Message {
	override val oid: Int = request.oid
	override def toString = s"Reply($request,${entries.size} entries,$errCode,$errMessage)"
}
case class Event(oid: Int, entries: List[Entry], errCode: Int, errMessage: String) extends Message {
	override def toString = s"Event($oid,${entries.size} entries,$errCode,$errMessage)"
}
