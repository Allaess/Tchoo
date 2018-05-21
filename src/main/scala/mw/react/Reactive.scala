package mw.react

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait Reactive[+T] {
	outer =>
	def onEvent(action: Event[T] => Unit): Registration
	def onData(action: T => Unit): Registration = onEvent {
		case Data(t) => action(t)
		case _ =>
	}
	def onFinal(action: => Unit): Registration = onEvent {
		case _: Final => action
		case _ =>
	}
	def onError(action: Throwable => Unit): Registration = onEvent {
		case Error(e) => action(e)
		case _ =>
	}
	def onClosed(action: => Unit): Registration = onEvent {
		case Closed => action
		case _ =>
	}
	def map[S](f: T => S): Reactive[S] = new EventSource[S] {
		outer.onEvent { event =>
			trigger(event.map(f))
		}
//		override def toString = s"${super.toString} <- $outer"
	}
	def flatMap[S](f: T => Reactive[S]): Reactive[S] = new EventSource[S] {
		private var count = 1
		outer.onEvent {
			case Data(t) => Try(f(t)) match {
				case Success(react) =>
					count += 1
					react.onEvent {
						case Closed => close()
						case event => trigger(event)
					}
				case Failure(error) => trigger(Error(error))
			}
			case error: Error => trigger(error)
			case Closed => close()
		}
		override def close(): Unit = {
			count -= 1
			if (count == 0) super.close()
		}
//		override def toString = s"${super.toString} <- $outer"
	}
	def flatten[S](implicit id: T => Reactive[S]): Reactive[S] = flatMap(id)
	def withFilter(p: T => Boolean): Reactive[T] = new EventSource[T] {
		outer.onEvent {
			case data@Data(t) => Try(p(t)) match {
				case Success(true) => trigger(data)
				case Success(false) =>
				case Failure(error) => trigger(Error(error))
			}
			case event: Final => trigger(event)
		}
//		override def toString = s"${super.toString} <- $outer"
	}
	def foreach(action: T => Unit): Unit = onData { t =>
		action(t)
	}
	def collect[S](pf: PartialFunction[T, S]): Reactive[S] = withFilter(pf.isDefinedAt).map(pf)
	def scan[S](init: S)(f: (S, T) => S): Reactive[S] = {
		var s = init
		outer.map { t =>
			s = f(s, t)
			s
		}
	}
	def switch[S](implicit id: T => Reactive[S]): Reactive[S] = new EventSource[S] {
		private var outerClosed = false
		private var innerClosed = true
		private var registration = Registration.empty
		outer.onEvent {
			case Data(t) =>
				registration.cancel()
				innerClosed = false
				registration = id(t).onEvent {
					case Closed =>
						innerClosed = true
						if (outerClosed) close()
					case event => trigger(event)
				}
			case error: Error => trigger(error)
			case Closed =>
				outerClosed = true
				if (innerClosed) close()
		}
//		override def toString = s"${super.toString} <- $outer"
	}
	def ++[S >: T](that: Reactive[S]): Reactive[S] = new EventSource[S] {
		private var count = 2
		outer.onEvent {
			case Closed => close()
			case event => trigger(event)
		}
		that.onEvent {
			case Closed => close()
			case event => trigger(event)
		}
		override def close(): Unit = {
			count -= 1
			if (count == 0) super.close()
		}
//		override def toString = s"${super.toString} <- ($outer,$that)"
	}
}
object Reactive {
	val silent: Reactive[Nothing] = { _ =>
		Registration.empty
	}
	def empty[T]: Reactive[T] = silent
	implicit def fromSeq[T](coll: Seq[T]): Reactive[T] = { action =>
		for (elem <- coll) {
			action(Data(elem))
		}
		action(Closed)
		Registration.empty
	}
	implicit def fromSet[T](coll: Set[T]): Reactive[T] = { action =>
		for (elem <- coll) {
			action(Data(elem))
		}
		action(Closed)
		Registration.empty
	}
	implicit def fromMap[K, V](coll: Map[K, V]): Reactive[(K, V)] = { action =>
		for (elem <- coll) {
			action(Data(elem))
		}
		action(Closed)
		Registration.empty
	}
	implicit def fromOption[T](option: Option[T]): Reactive[T] = { action =>
		for (elem <- option) {
			action(Data(elem))
		}
		action(Closed)
		Registration.empty
	}
}
