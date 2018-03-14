package prefixReversals

abstract class Component{
	def reverse: Component
	def contains(n: Int): Boolean
	def min: Int
	def max: Int
	def orientatedWith(that: Component): Boolean
}
case class Elem(val value: Int, val amount: Int = 1) extends Component{
	def reverse = this
	def contains(n: Int) = (value == n)
	def min = value
	def max = value
	def orientatedWith(that: Component) = true
	override def toString = value.toString
}
case class Block(val orientated: Boolean, val low: Elem, val high: Elem) extends Component{
	def reverse = Block(!orientated, low, high)
	def contains(n: Int) = (n >= low.value && n<= high.value)
	def min = low.value
	def max = high.value
	def toSeq = if (orientated) (low.value to high.value) else (low.value to high.value).reverse
	def orientatedWith(that: Component) = that match {
		case Elem(_, _) => true
		case Block(thatOrientated, _, _) => orientated == thatOrientated
	}
	override def toString = if (orientated) f"$low->$high" else f"$high<-$low"
}