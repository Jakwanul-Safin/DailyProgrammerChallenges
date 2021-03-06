package prefixReversals

object Counter{
	def apply(n: Int) = new Counter(n)
}
class Counter(var count: Int){
	def value = count
	override def toString = count.toString
	def decrement{
		count -= 1
	}
	override def clone = new Counter(count)
}

object PancakeStack{
	def apply(intStack: Vector[Int]) = {
		val key = intStack.sorted.distinct
		new PancakeStack(intStack map(key.indexOf(_)) map(Elem(_)),
		 key map(e => intStack.count(_ == e)) map(Counter(_)), key)
	}

	def naiveSolution(start: PancakeStack): Vector[Vector[Component]] = {
		val stack = start.stack
		if (stack.isEmpty) Vector(Vector())
		else {
			val max = stack.maxBy(_.max)
			val maxIndex = stack.indexOf(max)
			println(stack)
			println(max)
			println()
			if (stack.last.max == max.max && max.properlyOrientated) {
				val newCounter = start.counters.map(_.clone)
				max match {
					case Elem(a, _) => newCounter(a).decrement
					case Block(_, a, b) => {
						newCounter(a.value).decrement
						newCounter(b.value).decrement
					}
				}
				val next = new PancakeStack(stack.init, newCounter, Vector())
				naiveSolution(next).map(_:+max)
			} else maxIndex match {
				case 0 if (max.reverse.properlyOrientated) => {
					val next = start.swap(stack.length - 1)
					stack +: naiveSolution(next)
				}
				case _ => {
					val next = start.swap(maxIndex)
					stack +: naiveSolution(next)
				}
			}
		}
	}

}

class PancakeStack(val stack: Vector[Component], val counters: Vector[Counter], val key: Vector[Int]){
	override def toString = 
		"Stack: " + stack.mkString(",") +
	 	"\nCounts: " + counters.mkString(",") + 
	 	"\nKey: " + key.mkString(",")

	def decode(count: Vector[Counter] = counters) = {
		def decodePart(comp: Component) : Vector[Int]= comp match {
			case Elem(a, q) => Vector.fill(q)(a)
			case block@Block(orient, low, high) => {
				val sequence = block.toSeq.toVector
				decodePart(if (block.orientated) block.low else block.high) ++ 
				sequence.tail.init.map(a => Vector.fill(count(a).value)(a)).fold(Vector()) (_++_) ++
				decodePart(if (block.orientated) block.high else block.low)
			}
		}
		stack.map(decodePart).fold(Vector())(_++_).map(key(_))
	}

	def consecutive(first: Component, sec: Component) = (first, sec) match {
		case (Elem(a, _), Elem(b, _)) => 
			b == a || b == a + 1 && (count(a) == 1 || count(b) == 1) 
		case (Block(_, Elem(l, _), Elem(a, _)), Elem(b, _)) =>
			b == a || b == a + 1 && count(a) == 1 
		case (Elem(a, _), Block(_, Elem(b, _), Elem(h, _))) =>
			b == a || b == a + 1 && count(b) == 1 
		case (Block(_, _, Elem(a, _)), Block(_, Elem(b, _), _)) => 
			b == a && count(b) == 2 || b == a + 1 && count(a) == 1 && count(b) == 1
		case (_, _) => false
	}

	def count(n: Int) = counters(n).value

	def combine(first: Component, second: Component, counters: Vector[Counter]): Option[Component] ={
		def helper(a: Elem, b: Elem) = if (a.value == b.value) Elem(a.value, b.amount + a.amount) else a
		(first, second) match {
			case (Elem(a, qa), Elem(b, qb)) if (a == b) => {
				counters(a).decrement
				Some(Elem(a, qa + qb))
			}
			case (first: Elem, second: Elem) =>{
				if (consecutive(first, second)) Some(Block(true, first, second)) 
				else if (consecutive(second, first)) Some(Block(false, second, first))
				else None
			}
			case (Block(orient, low, mida), Block(thatOrient, midb, high)) if (orient == thatOrient) => {
				if (if (orient) consecutive(first, second) else consecutive(second, first)) {
					counters(mida.value).decrement
					counters(midb.value).decrement
					Some(Block(orient, low, high))
				}
				else None
			}
			case (Block(orient, low, mid), high: Elem) => {
				if (orient && consecutive(first, second)) {
					counters(mid.value).decrement
					Some(Block(orient, low, helper(high, mid)))
				} else if (consecutive(second, first)){
					counters(low.value).decrement
					Some(Block(orient, high, helper(mid, low)))
				}
				else None
			}
			case (low: Elem, Block(orient, mid, high)) => {
				if (orient && consecutive(first, second)) {
					counters(mid.value).decrement
					Some(Block(orient, low, helper(high, mid)))
				} else if (consecutive(second, first)){
					counters(high.value).decrement
					Some(Block(orient, mid, helper(low, high)))
				}
				else None
			}
			case _ => None
		}
	}

	def compressed = {
		val newCounter = counters map(_.clone)
		def compressor(acc: (Component, Vector[Component]), next: Component): (Component, Vector[Component]) = {
			val (partial, chain) = acc
			combine(partial, next, newCounter) match {
				case Some(nextPartial) => (nextPartial, chain)
				case None => (next, chain :+ partial)
			}
		}

		val (last, chain) = stack.tail.foldLeft(stack.head: Component, Vector(): Vector[Component]) (compressor)
		new PancakeStack(chain :+ last, newCounter, key)
	}

	def swap(index: Int) = {
		def flip(li: Vector[Component]) = li.foldLeft(Vector(): Vector[Component]) ((acc, e) => e.reverse +: acc)
		if (index == stack.length - 1) new PancakeStack(flip(stack), counters.map(_.clone), key)
		else {
			val (a, b) = (stack(0).reverse, stack(index + 1))
			val newCounter = counters map(_.clone)
			combine(a, b, newCounter) match {
				case Some(combined) => {
					val newStack = (flip(stack.slice(1, index + 1)) :+ combined) ++
						stack.slice(index + 2, stack.length)
					new PancakeStack(newStack, newCounter, key)
				}
				case None => {
					val newStack = flip(stack.slice(0, index + 1)) ++ stack.slice(index + 1, stack.length)
					new PancakeStack(newStack, counters, key)
				}
		}
		}
	}

}

object PancakeStackUnitTest{
	def main(args: Array[String]){
		val testVector = Vector(7,6,4,2,6,7,8,7)
		println("Testing with: " + testVector.mkString(","))

		val testStack = PancakeStack(testVector)
		println("\nBefore Compression \n" + testStack + "\n")
		println("'First two elements are consecutive' is " + testStack.consecutive(testStack.stack(0), testStack.stack(1)))
		println("'Second two elements are consecutive' is " + testStack.consecutive(testStack.stack(1), testStack.stack(2)))

		println("\nAfter Compression: \n" + testStack.compressed)

		val swapIndex = 2
		val transformed = testStack.compressed.swap(swapIndex)
		println(f"\nTesting Swap at Index $swapIndex: \n" + transformed.toString)
		println("Decoded that is: \n" + transformed.decode(testStack.counters).mkString(","))

		val solution = PancakeStack.naiveSolution(testStack.compressed).map(new PancakeStack(_, testStack.counters, testStack.key)).map(_.decode())
		println("Solution is: \n" + solution.mkString("\n"))
	}
}