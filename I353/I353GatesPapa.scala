object ComponentUnitTest{
	def main(args: Array[String]){
		println(blockify(Vector(1, 2, 4, 3, 5, 6)))
	}

	class Counter(var count: Int){
		def value = count
		def decrement{count -= 1}
	}

	def sortWithCount(li: Vector[Int]) = li.sorted.distinct map{x => (x, new Counter(li.count(_==x)))}

	def ordinalMap(stack: Vector[Int]) = {
		
	}

	def blockify(stack: Vector[Int]) = {

		def groupBlock(acc: (Component, Vector[Component]), next: Component): (Component, Vector[Component]) = {
			val (partial, chain) = acc
			val nextPartial: Component = (partial, next) match {
				case (Elem(a), Elem(b)) if (b - a == 1) => Block(true, a, b)
				case (Elem(a), Elem(b)) if (a - b == 1) => Block(false, b, a)
				case (Elem(b), Block(true, l, h)) if (l - b == 1) => Block(true, b, h)
				case (Elem(b), Block(false, l, h)) if (b - h == 1) => Block(false, l, b)
				case (Block(true, l, h), Elem(b)) if (b - h == 1) => Block(true, l, b)
				case (Block(false, l, h), Elem(b)) if (l - b == 1) => Block(false, b, h)
				case (Block(true, l, a), Block(true, b, h)) if (b - a == 1) => Block(true, l, h)
				case (Block(false, a, h), Block(false, l, b)) if (a - b == 1) => Block(false, l, h)
				case _ => Elem(-1)
			}

			if (nextPartial == Elem(-1)) (next, chain :+ partial) else (nextPartial, chain)
		}

		val components =(stack map(Elem(_)))
		val (last, chain) = components.tail.foldLeft((Elem(stack.head): Component, Vector(): Vector[Component])) (groupBlock)
		chain :+ last
	}
}

abstract class Component
case class Elem(val value: Int) extends Component
case class Block(val orientated: Boolean, val low: Int, val high: Int) extends Component{
	def reverse = Block(!orientated, low, high)
	def toList = (low to high by {if (orientated) 1 else -1})
}