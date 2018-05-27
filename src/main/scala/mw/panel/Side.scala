package mw.panel

import mw.hetero.{Decode, DecodeException}

sealed trait Side {
	def other: Side
}
object Side {
	implicit val decode: Decode[Side] = new Decode[Side] {
		def minSize = 1
		def apply(data: List[String]) = data match {
			case head :: tail => (Side(head), tail)
			case Nil => throw DecodeException("No data left for Side")
		}
	}
	def apply(name: String): Side = name match {
		case "entry" => Entry
		case "exit" => Exit
	}
}
case object Entry extends Side {
	def other = Exit
}
case object Exit extends Side {
	def other = Entry
}
