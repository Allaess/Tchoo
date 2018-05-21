package mw.react

import scala.util.{Failure, Success, Try}

sealed trait Event[+T] {
	def map[S](f: T => S): Event[S]
	def flatMap[S](f: T => Event[S]): Event[S]
	def flatten[S](implicit id: T => Event[S]): Event[S] = flatMap(id)
}
object Event {
	def apply[T](expr: => T): Event[T] = Try(expr) match {
		case Success(t) => Data(t)
		case Failure(e) => Error(e)
	}
}
case class Data[+T](value: T) extends Event[T] {
	override def map[S](f: T => S): Event[S] = Event(f(value))
	override def flatMap[S](f: T => Event[S]): Event[S] = Try(f(value)) match {
		case Success(event) => event
		case Failure(error) => Error(error)
	}
}
sealed trait Final extends Event[Nothing] {
	override def map[S](f: Nothing => S): this.type = this
	override def flatMap[S](f: Nothing => Event[S]): this.type = this
}
case class Error(error: Throwable) extends Final
case object Closed extends Final
