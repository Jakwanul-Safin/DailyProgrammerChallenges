package prefixReversals
//import component.scala;
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
}
class PancakeStack(val stack: Vector[Component], counters: Vector[Counter], key: Vector[Int]){
	override def toString = "Stack: " + stack.mkString(",") +
	 "\nCounts: " + counters.mkString(",") + "\nKey: " + key.mkString(",")

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

	def compressed = {
		val newCounter = counters map(_.clone)
		def compressor(acc: (Component, Vector[Component]), next: Component): (Component, Vector[Component]) = {
			val (partial, chain) = acc
			if (next.orientatedWith(partial) && (consecutive(partial, next) || consecutive(next, partial))) {
				val nextPartial = (partial, next) match {
					case (Elem(a, qa), Elem(b, qb)) if (a == b) => {
						newCounter(a).decrement
						Elem(a, qa + qb)
					}
					case (Elem(a, qa), Elem(b, qb)) =>{
						if (b == a + 1) Block(true, Elem(a, qa), Elem(b, qb)) 
							else Block(false, Elem(b, qb), Elem(a, qa))
					}
					case (Block(true, low, mida), Block(_, midb, high)) => {
						newCounter(mida.value).decrement
						newCounter(midb.value).decrement
						Block(true, low, high)
					}
					case (Block(false, mida, high), Block(_, low, midb)) => {
						newCounter(mida.value).decrement
						newCounter(midb.value).decrement
						Block(false, low, high)
					}
					case (Block(true, low, mid), high: Elem) =>{
						newCounter(mid.value).decrement
						Block(true, low, high)
					}
					case (Block(false, mid, high), low: Elem) => {
						newCounter(mid.value).decrement
						Block(false, low, high)
					}
					case (low: Elem, Block(true, mid, high)) =>{
						newCounter(mid.value).decrement
						Block(true, low, high)
					}
					case (high: Elem, Block(false, low, mid)) => {
						newCounter(mid.value).decrement
						Block(true, low, high)
					}
				}
				(nextPartial, chain)
			} else (next, chain :+ partial)
		}

		val (last, chain) = stack.tail.foldLeft(stack.head: Component, Vector(): Vector[Component]) (compressor)
		new PancakeStack(chain :+ last, newCounter, key)
	}

	def swap(index: Int) = {}
}

object PancakeStackUnitTest{
	def main(args: Array[String]){
		val testVector = Vector(7,8,2,7,7,4,3,100)
		println("Testing with: " + Vector(7,8,2,7,7,4,3,100).mkString(","))

		val testStack = PancakeStack(testVector)
		println("\n Before Compression \n" + testStack + "\n")
		println("'First two elements are consecutive' is " + testStack.consecutive(testStack.stack(0), testStack.stack(1)))
		println("'Second two elements are consecutive' is " + testStack.consecutive(testStack.stack(1), testStack.stack(2)))

		println("\n After Compression: " + testStack.compressed)
	}
}