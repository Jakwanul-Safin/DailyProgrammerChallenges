package prefixReversals

abstract class Component{
	def reverse: Component
	def contains(n: Int): Boolean
	def min: Int
	def max: Int
}
case class Elem(val value: Int, val amount: Int = 1) extends Component{
	def reverse = this
	def contains(n: Int) = (value == n)
	def min = value
	def max = value
	override def toString = value.toString
}
case class Block(val orientated: Boolean, val low: Elem, val high: Elem) extends Component{
	def reverse = Block(!orientated, low, high)
	def contains(n: Int) = (n >= low.value && n<= high.value)
	def min = low.value
	def max = high.value
	def toList = (low.value to high.value by {if (orientated) 1 else -1})
	override def toString = low.toString + {if (orientated) "->" else "<-"} + high.toString
}