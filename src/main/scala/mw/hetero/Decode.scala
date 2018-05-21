package mw.hetero

import scala.util.{Failure, Success, Try}

trait Decode[+T] {
	def apply(data: List[String]): (T, List[String])
	def minSize: Int
}
object Decode {
	def apply[T](data: List[String])(implicit decode: Decode[T]): T = decode(data) match {
		case (t, Nil) => t
		case (t, rest) =>
			System.err.println(s"Ignored data: $rest")
			t
	}
	implicit val byte: Decode[Byte] = new Decode[Byte] {
		override val minSize = 1
		override def apply(data: List[String]) = data match {
			case num :: rest => (num.toByte, rest)
			case Nil => throw DecodeException("No data available for Byte")
		}
	}
	implicit val short: Decode[Short] = new Decode[Short] {
		override val minSize = 1
		override def apply(data: List[String]) = data match {
			case num :: rest => (num.toShort, rest)
			case Nil => throw DecodeException("No data available for Short")
		}
	}
	implicit val int: Decode[Int] = new Decode[Int] {
		override val minSize = 1
		override def apply(data: List[String]) = data match {
			case num :: rest => (num.toInt, rest)
			case Nil => throw DecodeException("No data available for Int")
		}
	}
	implicit val long: Decode[Long] = new Decode[Long] {
		override val minSize = 1
		override def apply(data: List[String]) = data match {
			case num :: rest => (num.toLong, rest)
			case Nil => throw DecodeException("No data available for Long")
		}
	}
	implicit val float: Decode[Float] = new Decode[Float] {
		override val minSize = 1
		override def apply(data: List[String]) = data match {
			case num :: rest => (num.toFloat, rest)
			case Nil => throw DecodeException("No data available for Float")
		}
	}
	implicit val double: Decode[Double] = new Decode[Double] {
		override val minSize = 1
		override def apply(data: List[String]) = data match {
			case num :: rest => (num.toDouble, rest)
			case Nil => throw DecodeException("No data available for Double")
		}
	}
	implicit val char: Decode[Char] = new Decode[Char] {
		override val minSize = 1
		override def apply(data: List[String]) = data match {
			case car :: rest if car.length == 1 => (car.charAt(0), rest)
			case car :: _ => throw DecodeException(s"$car is not a single Char")
			case Nil => throw DecodeException("No data available for Char")
		}
	}
	implicit val boolean: Decode[Boolean] = new Decode[Boolean] {
		override val minSize = 1
		override def apply(data: List[String]) = data match {
			case bool :: rest => (bool.toBoolean, rest)
			case Nil => throw DecodeException("No data available for Boolean")
		}
	}
	implicit val string: Decode[String] = new Decode[String] {
		override def minSize = 1
		override def apply(data: List[String]) = data match {
			case text :: rest => (text, rest)
			case Nil => throw DecodeException("No data available for String")
		}
	}
	implicit def tuple2[T1, T2](implicit decode1: Decode[T1], decode2: Decode[T2]): Decode[(T1, T2)] =
		new Decode[(T1, T2)] {
			override def minSize = decode1.minSize + decode2.minSize
			override def apply(data: List[String]) = {
				val (t1, rest1) = decode1(data)
				val (t2, rest2) = decode2(rest1)
				((t1, t2), rest2)
			}
		}
	implicit def tuple3[T1, T2, T3](implicit decode1: Decode[T1], decode2: Decode[T2], decode3: Decode[T3]):
	Decode[(T1, T2, T3)] = new Decode[(T1, T2, T3)] {
		override def minSize = decode1.minSize + decode2.minSize + decode3.minSize
		override def apply(data: List[String]) = {
			val (t1, rest1) = decode1(data)
			val (t2, rest2) = decode2(rest1)
			val (t3, rest3) = decode3(rest2)
			((t1, t2, t3), rest3)
		}
	}
	implicit def list[T](implicit decode: Decode[T]): Decode[List[T]] = new Decode[List[T]] {
		override val minSize = 0
		override def apply(data: List[String]) = {
			Try(decode(data)) match {
				case Success((head, rest)) => Try(list(decode)(rest)) match {
					case Success((tail, rest2)) => (head :: tail, rest2)
					case Failure(_) => (head :: Nil, rest)
				}
				case Failure(_) => (Nil, data)
			}
		}
	}
	implicit def set[T](implicit decode: Decode[T]): Decode[Set[T]] = new Decode[Set[T]] {
		override val minSize = 0
		override def apply(data: List[String]) = {
			val (result, rest) = list(decode)(data)
			(result.toSet, rest)
		}
	}
	implicit def map[K, V](implicit decode: Decode[(K, V)]): Decode[Map[K, V]] = new Decode[Map[K, V]] {
		override def minSize = 0
		override def apply(data: List[String]) = {
			val (result, rest) = list(decode)(data)
			(result.toMap, rest)
		}
	}
	implicit def option[T](implicit decode: Decode[T]): Decode[Option[T]] = new Decode[Option[T]] {
		override def minSize = decode.minSize
		override def apply(data: List[String]) = {
			Try(decode(data)) match {
				case Success((result, rest)) => (Some(result), rest)
				case Failure(_) => (None, data.drop(minSize))
			}
		}
	}
	implicit def hList[H, T <: HList](implicit headDecode: Decode[H], tailDecode: Decode[T]): Decode[H :: T] =
		new Decode[H :: T] {
			override def minSize = headDecode.minSize + tailDecode.minSize
			override def apply(data: List[String]) = {
				val (head, rest) = headDecode(data)
				val (tail, rest2) = tailDecode(rest)
				(head :: tail, rest2)
			}
		}
	implicit val hNil: Decode[HNil] = new Decode[HNil] {
		override def minSize = 0
		override def apply(data: List[String]) = (HNil, data)
	}
}
