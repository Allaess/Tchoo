package mw.tchoo

import mw.hetero.Decode

import scala.util.Try

sealed trait Line
sealed trait StartLine extends Line
case class EventLine(oid: Int) extends StartLine
case class ReplyLine(request: Request) extends StartLine
case class Entry(oid: Int, args: List[Argument]) extends Line {
	def has(names: String*): Boolean = {
		val found = for (name <- names) yield {
			args.exists(_.name == name)
		}
		(true /: found) (_ && _)
	}
	def get[T](name: String)(implicit decode: Decode[T]): Option[T] = {
		args.find(_.name == name).flatMap { arg =>
			Try(Decode[T](arg.values)).toOption
		}
	}
}
case class EndLine(errCode: Int, errMessage: String) extends Line
