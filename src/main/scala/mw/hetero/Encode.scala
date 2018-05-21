package mw.hetero

trait Encode[-T] {
	def apply(data: T, encoded: List[String]): List[String]
}
object Encode {
	def apply[T](value: T)(implicit encode: Encode[T]): List[String] = encode(value, Nil)
	implicit val byte: Encode[Byte] = {
		case (num, rest) => num.toString :: rest
	}
	implicit val short: Encode[Short] = {
		case (num, rest) => num.toString :: rest
	}
	implicit val int: Encode[Int] = {
		case (num, rest) => num.toString :: rest
	}
	implicit val long: Encode[Long] = {
		case (num, rest) => num.toString :: rest
	}
	implicit val float: Encode[Float] = {
		case (num, rest) => num.toString :: rest
	}
	implicit val double: Encode[Double] = {
		case (num, rest) => num.toString :: rest
	}
	implicit val boolean: Encode[Boolean] = {
		case (flag, rest) => flag.toString :: rest
	}
	implicit val char: Encode[Char] = {
		case (car, rest) => car.toString :: rest
	}
	implicit val string: Encode[String] = {
		case (text, rest) => text :: rest
	}
	implicit def tuple2[T1, T2](implicit encode1: Encode[T1], encode2: Encode[T2]): Encode[(T1, T2)] = {
		case ((t1, t2), rest) => encode1(t1, encode2(t2, rest))
	}
	implicit def tuple3[T1, T2, T3](implicit encode1: Encode[T1], encode2: Encode[T2], encode3: Encode[T3]):
	Encode[(T1, T2, T3)] = {
		case ((t1, t2, t3), rest) => encode1(t1, encode2(t2, encode3(t3, rest)))
	}
	implicit def option[T](implicit encode: Encode[T]): Encode[Option[T]] = {
		case (Some(t), rest) => encode(t, rest)
		case (None, rest) => "" :: rest
	}
	def list[T](encode: Encode[T]): Encode[List[T]] = {
		case (Nil, rest) => rest
		case (head :: tail, rest) => encode(head, list(encode)(tail, rest))
	}
	implicit def seq[T](implicit encode: Encode[T]): Encode[Seq[T]] = {
		case (seq, rest) => list(encode)(seq.toList, rest)
	}
	implicit def set[T](implicit encode: Encode[T]): Encode[Set[T]] = {
		case (set, rest) => list(encode)(set.toList, rest)
	}
	implicit def map[K, V](implicit encode: Encode[(K, V)]): Encode[Map[K, V]] = {
		case (map, rest) => list(encode)(map.toList, rest)
	}
	implicit def hList[H, T <: HList](implicit headEncode: Encode[H], tailEncode: Encode[T]): Encode[H :: T] = {
		case (head :: tail, rest) => headEncode(head, tailEncode(tail, rest))
	}
	implicit val hNil: Encode[HNil] = {
		case (_, rest) => rest
	}
}
