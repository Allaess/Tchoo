package mw.panel

case class Color(red: Int, green: Int, blue: Int) {
	def |(that: Color): Color = Color(this.red | that.red, this.green | that.green, this.blue | that.blue)
	override def toString: String = s"$red,$green,$blue"
}
object Color {
	val empty = Color(0, 0, 0)
}
