package mw.tchoo

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import java.net.Socket

import mw.hetero.Decode
import mw.react.{EventSource, Reactive}

import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.RegexParsers

class Ecos(name: String, port: Int) {
	private val socket = new Socket(name, port)
	private val input = new BufferedReader(new InputStreamReader(socket.getInputStream))
	private val output = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
	private val strings = EventSource[String]
	private val lines = strings.map { text =>
		Try(Ecos.parseLine(text))
	}
	for (Failure(error) <- lines) {
		error.printStackTrace()
	}
	val messages: Reactive[Message] =
		lines.scan(Option.empty[StartLine], List.empty[Entry], Option.empty[EndLine]) {
			case (_, Success(start: StartLine)) => (Some(start), Nil, None)
			case ((start@Some(_), entries, None), Success(entry: Entry)) => (start, entry :: entries, None)
			case ((start@Some(_), entries, None), Success(end: EndLine)) => (start, entries, Some(end))
			case _ => (None, Nil, None)
		}.collect {
			case (Some(start), entries, Some(end)) => Message(start, entries.reverse, end)
		}
	for {
		Message(_, _, code, message) <- messages
		if code != 0
	} {
		System.err.println(s"Error $code: $message")
	}
	private var oids = Set.empty[Int]
	private def view(oid: Int): Unit = if (!oids.contains(oid)) {
		oids += oid
		send(s"request($oid,view)")
	}
	private var requests = Set.empty[Request]
	private def ask(request: Request): Unit = if (!requests.contains(request)) {
		requests += request
		view(request.oid)
		send(request)
	}
	private var replies = Map.empty[Request, Reactive[List[Entry]]]
	def reply(REQUEST: Request): Reactive[List[Entry]] = replies.get(REQUEST) match {
		case Some(react) => react
		case None =>
			val react = for {
				Reply(REQUEST, entries, 0, _) <- messages
			} yield {
				entries
			}
			replies += REQUEST -> react
			ask(REQUEST)
			react
	}
	private var entries = Map.empty[(Int, String), Reactive[List[Entry]]]
	def entries(request: Request, name: String): Reactive[List[Entry]] = {
		val OID = request.oid
		entries.get(OID, name) match {
			case Some(react) => react
			case None =>
				val react = for (Message(OID, entries, 0, _) <- messages) yield {
					for (entry <- entries if entry.has(name)) yield {
						entry
					}
				}
				entries += (OID, name) -> react
				ask(request)
				react
		}
	}
	private var values = Map.empty[(Int, String), Reactive[Any]]
	def value[T](request: Request, NAME: String)(implicit decode: Decode[T]): Reactive[T] = {
		val OID = request.oid
		values.get(OID, NAME) match {
			case Some(react) => react.asInstanceOf[Reactive[T]]
			case None =>
				val react = for {
					Message(OID, entries, 0, _) <- messages
					Entry(OID, args) <- entries
					Argument(NAME, values) <- args
				} yield {
					Decode[T](values)
				}
				values += (OID, NAME) -> react
				ask(request)
				react
		}
	}
	def run(): Unit = {
		var line = input.readLine
		while (line != null) {
			println(s"Ecos => $line")
			strings() = line
			line = input.readLine
		}
	}
	def send(request: Request): Unit = {
		println(s"Ecos <= $request")
		output.write(request.toString)
		output.newLine()
		output.flush()
	}
	def send(text: String): Unit = send(Request(text))
	override def toString = s"Ecos($name:$port)"
}
object Ecos extends RegexParsers {
	def apply(name: String, port: Int = 15471): Ecos = new Ecos(name, port)
	def parseLine(text: String): Line = parseAll(line, text) match {
		case Success(line, _) => line
		case NoSuccess(message, _) => throw ParseException(message)
	}
	def parseRequest(text: String): Request = parseAll(request, text) match {
		case Success(request, _) => request
		case NoSuccess(message, _) => throw ParseException(message)
	}
	private def line: Parser[Line] = eventLine | replyLine | entryLine | endLine
	private def eventLine: Parser[EventLine] = "<" ~> "EVENT" ~> num <~ ">" ^^ EventLine
	private def replyLine: Parser[ReplyLine] = "<" ~> "REPLY" ~> request <~ ">" ^^ ReplyLine
	private def entryLine: Parser[Entry] = num ~ argument.* ^^ {
		case oid ~ args => Entry(oid, args)
	}
	private def endLine: Parser[EndLine] = "<" ~> "END" ~> num ~ ("(" ~> error <~ ")") <~ ">" ^^ {
		case errCode ~ errMessage => EndLine(errCode, errMessage)
	}
	private def num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
	private def request: Parser[Request] = keyword ~ ("(" ~> num ~ ("," ~> argument).* <~ ")") ^^ {
		case cmd ~ (oid ~ args) => Request(cmd, oid, args)
	}
	private def argument: Parser[Argument] = keyword ~ ("[" ~> (value ~ ("," ~> value).*).? <~ "]").? ^^ {
		case option ~ None => Argument(option, Nil)
		case option ~ Some(None) => Argument(option, Nil)
		case option ~ Some(Some(head ~ tail)) => Argument(option, head :: tail)
	}
	private def error: Parser[String] = """[^\(\)]+""".r
	private def keyword: Parser[String] = """[a-zA-Z0-9_\-]+""".r
	private def value: Parser[String] = stringValue | otherValue
	private def stringValue: Parser[String] = """"[^\"]*"""".r ^^ (_.drop(1).dropRight(1))
	private def otherValue: Parser[String] ="""[^\"\[\]\,]+""".r
}
