package mw.hetero

sealed trait HList {
	def ::[H](head: H): H :: this.type = new ::(head, this)
}
class ::[+H, +T <: HList](val head: H, val tail: T) extends HList {
	override def equals(that: scala.Any): Boolean = that match {
		case that: ::[_, _] => this.head == that.head && this.tail == that.tail
		case _ => false
	}
	override def hashCode(): Int = head.hashCode() + tail.hashCode()
	override def toString = s"$head :: $tail"
}
object :: {
	def unapply[L, H, T](list: L)(implicit f: Unapply[L, H, T]): Option[(H, T)] = f(list)
}
sealed trait HNil extends HList
case object HNil extends HNil
trait Unapply[-L, +H, +T] extends (L => Option[(H, T)])
object Unapply {
	implicit def list[E]: Unapply[List[E], E, List[E]] = { list =>
		if (list.isEmpty) None
		else Some(list.head, list.tail)
	}
	implicit def hList[H, T <: HList]: Unapply[H :: T, H, T] = { list =>
		Some(list.head, list.tail)
	}
	implicit val hNil: Unapply[HNil, Nothing, Nothing] = { _ =>
		None
	}
}
