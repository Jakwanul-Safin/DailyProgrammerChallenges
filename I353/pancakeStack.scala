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
}

object PancakeStack{
	def apply(intStack: Vector[Int]) = {
		val key = intStack.sorted.distinct
		new PancakeStack(intStack map(key.indexOf(_)) map(Elem(_)),
		 key map(e => intStack.count(_ == e)) map(Counter(_)), key)
	}
}
class PancakeStack(stack: Vector[Component], count: Vector[Counter], key: Vector[Int]){
	override def toString = "Stack: " + stack.mkString(",") +
	 "\nCounts: " + count.mkString(",") + "\nKey: " + key.mkString(",")

}

object PancakeStackUnitTest{
	def main(args: Array[String]){
		val testVector = Vector(7,8,2,7,7,4,3,100)
		println("Testing with: " + Vector(7,8,2,7,7,4,3,100).mkString(","))
		val testStack = PancakeStack(testVector)
		println(testStack)
	}
}