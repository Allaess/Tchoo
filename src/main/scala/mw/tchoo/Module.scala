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
			val request = Request(s"get($oid,state,railcom)")
			val module = new Module {
				val state = for {
					entries <- ecos.entries(request, "state")
					entry <- entries
					value <- entry.get[String]("state")
				} yield {
					value.drop(2).fromHex
				}
				val railcom = for {
					entries <- ecos.entries(request, "railcom")
				} yield {
					val list = for {
						entry <- entries
						(port, address, orientation) <- entry.get[(Int, Int, Int)]("railcom") if address != 0
					} yield {
						port -> (address, orientation != 0)
					}
					list.toMap
				}
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
