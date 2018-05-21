package mw.react

trait EventSource[T] extends Reactive[T] {
	private var actions = Map.empty[Event[T] => Unit, Int]
	private var lastData = Option.empty[Data[T]]
	private var finalEvent = Option.empty[Final]
	def trigger(event: Event[T]): Unit = if (finalEvent.isEmpty) {
		val saved = actions
		event match {
			case data: Data[T] =>
				lastData = Some(data)
			case event: Final =>
				finalEvent = Some(event)
				actions = Map.empty
		}
		for ((action, _) <- saved) {
			action(event)
		}
	}
	def update(expr: => T): Unit = trigger(Event(expr))
	def close(): Unit = trigger(Closed)
	def onEvent(action: Event[T] => Unit): Registration = finalEvent match {
		case None =>
			actions.get(action) match {
				case None => actions += action -> 1
				case Some(count) => actions += action -> (count + 1)
			}
			for (data <- lastData) {
				action(data)
			}
			Registration {
				actions.get(action) match {
					case Some(1) => actions -= action
					case Some(count) => actions += action -> (count - 1)
					case None =>
				}
			}
		case Some(event) =>
			for (data <- lastData) {
				action(data)
			}
			action(event)
			Registration.empty
	}
	override def toString: String = lastData match {
		case Some(Data(t)) => s"React($t)"
		case None => "React()"
	}
}
object EventSource {
	def apply[T]: EventSource[T] = new EventSource[T] {}
}
