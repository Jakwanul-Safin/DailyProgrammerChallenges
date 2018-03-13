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
		case (_, Elem(b, _)) => {
			val a = first.max
			b == a || ((count(a) == 1 || count(b) == 1) && b == a + 1) 
		}
		case (Elem(a, _), _) => {
			val b = sec.min
			b == a || ((count(a) == 1 || count(b) == 1) && b == a + 1) 
		}
		case (Block(_, _, Elem(a, _)), Block(_, Elem(b, _), _)) => (count(a) == 1 && count(b) == 1 && b == a + 1)
		case (_, _) => false
	}

	def count(n: Int) = counters(n).value
}

object PancakeStackUnitTest{
	def main(args: Array[String]){
		val testVector = Vector(7,8,2,7,7,4,3,100)
		println("Testing with: " + Vector(7,8,2,7,7,4,3,100).mkString(","))
		val testStack = PancakeStack(testVector)
		println(testStack)
		println("'First two elements consecutive' is " + testStack.consecutive(testStack.stack(0), testStack.stack(1)))
		println("'Second two elements consecutive' is " + testStack.consecutive(testStack.stack(1), testStack.stack(2)))
	}
}