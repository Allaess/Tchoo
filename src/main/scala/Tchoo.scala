import mw.panel.Panel
import mw.tchoo.Ecos

object Tchoo extends App {
	val ecos = Ecos("192.168.1.68")
	val panel = Panel(ecos, "hidden")
	ecos.run()
}
