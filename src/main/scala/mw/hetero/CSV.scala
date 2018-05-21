package mw.hetero

import scala.io.{Codec, Source}
import scala.util.{Success, Try}

object CSV {
	implicit val codec: Codec = Codec.UTF8
	def read[T](fileName: String)(implicit decode: Decode[T]): Iterator[T] = {
		val loader = Thread.currentThread.getContextClassLoader
		val input = loader.getResourceAsStream(fileName)
		read(Source.fromInputStream(input))
	}
	def read[T](source: Source)(implicit decode: Decode[T]): Iterator[T] = {
		source.getLines.drop(1).withFilter(_.nonEmpty).map { line =>
			Try(decode(line.split(";").map(_.trim).toList))
		}.collect {
			case Success((t, Nil)) => t
		}
	}
}
