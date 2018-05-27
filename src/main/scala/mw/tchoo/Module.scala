package mw.tchoo

import mw.react.Reactive

trait Module {
	val state: Reactive[Int]
	val railcom: Reactive[Map[Int, (Int, Boolean)]]
	override def toString = s"Module(state=$state,railcom=$railcom)"
}
object Module {
	private var modules = Map.empty[(Ecos, Int), Module]
	def apply(ecos: Ecos, oid: Int): Module = modules.get(ecos, oid) match {
		case Some(module) => module
		case None =>
			val module = new Module {
				val request = Request(s"get($oid,state,railcom)")
				val state = for (value <- ecos.values[String](request, "state")) yield {
					value.drop(2).fromHex
				}
				val railcom = {
					val pairs = for ((port, address, orientation) <- ecos.values[(Int, Int, Int)](request, "railcom")) yield {
						port -> (address, orientation != 0)
					}
					pairs.scan(Map.empty[Int, (Int, Boolean)]) { (map, pair) =>
						val address = pair._2._1
						if (address == 0) map - pair._1
						else map + pair
					}
				}.debounce
			}
			modules += (ecos, oid) -> module
			module
	}
	implicit class StringOps(text: String) {
		def fromHex: Int = if (text.isEmpty) 0
		else text.dropRight(1).fromHex * 16 + text.last.toUpper match {
			case '0' => 0
			case '1' => 1
			case '2' => 2
			case '3' => 3
			case '4' => 4
			case '5' => 5
			case '6' => 6
			case '7' => 7
			case '8' => 8
			case '9' => 9
			case 'A' => 10
			case 'B' => 11
			case 'C' => 12
			case 'D' => 13
			case 'E' => 14
			case 'F' => 15
		}
	}
}
