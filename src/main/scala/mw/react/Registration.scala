package mw.react

trait Registration {
	def cancel(): Unit
	def +(that: Registration): Registration = { () =>
		this.cancel()
		that.cancel()
	}
}
object Registration {
	val empty: Registration = { () => }
	def apply(action: => Unit): Registration = { () => action }
	def variable: Var = new Var {
		private var registration = empty
		override def apply() = registration
		override def update(that: Registration) = registration = that
		override def cancel() = registration.cancel()
	}
	trait Var extends Registration {
		def apply(): Registration
		def update(that: Registration): Unit
	}
}
