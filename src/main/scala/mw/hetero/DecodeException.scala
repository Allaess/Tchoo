package mw.hetero

class DecodeException(message: String) extends Exception(message)
object DecodeException {
	def apply(message: String): DecodeException = new DecodeException(message)
}
