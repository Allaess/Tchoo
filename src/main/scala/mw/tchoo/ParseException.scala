package mw.tchoo

class ParseException(message: String) extends Exception(message)
object ParseException {
	def apply(message: String): ParseException = new ParseException(message)
}
