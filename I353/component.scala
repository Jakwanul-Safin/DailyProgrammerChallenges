package prefixReversals

abstract class Component{
	def reverse: Component
	def contains(n: Int): Boolean
}
case class Elem(val value: Int) extends Component{
	def reverse = this
	def contains(n: Int) = (value == n)
	override def toString = value.toString
}
case class Block(val orientated: Boolean, val low: Int, val high: Int) extends Component{
	def reverse = Block(!orientated, low, high)
	def contains(n: Int) = (n >= low && n<= high)
	def toList = (low to high by {if (orientated) 1 else -1})
	override def toString = low.toString + {if (orientated) "->" else "<-"} + high.toString
}