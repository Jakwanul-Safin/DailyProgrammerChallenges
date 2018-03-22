package o

object Expansion{
	def main(args: Array[String]){
		println(primeComplexities(100))
	}

	def complexity(n: Int) = {
		StoredValues.buildTo(n)
		StoredValues.complexity(n) match {
			case Some(comp) => comp 
			case None => 0
		}
	}

	def expand(n: Int): OpPair = {
		val nComp = complexity(n)
		val loop = StoredValues.divisorPairs(n).filter(p => complexity(p._1) + complexity(p._2) == nComp)
		if (loop.isEmpty){
			if (n == 1) Empty()
			else Sum(n - 1, 1)
		}
		else {
			val (a, b): (Int, Int) = loop.head
			Prod(a, b)
		}
	}

	def primeComplexities(limit: Int) = (1 to limit).filter(n =>
		expand(n) match {
			case Sum(_, _) => true
			case _ => false
		}
	)

	def log3(n: BigInt): Int = if (n < 3) 0 else (1 + log3(n/3))
}

abstract class OpPair{
	def complexity: Int
	def number: Int
	def accuracy = complexity - 3*(Math.log(number)/Math.log(3)).toInt
	def decomposition: String
}

case class Prod(a: Int, b: Int) extends OpPair{
	require(a >= b)
	var savedComplexity = -1
	def complexity ={
		if (savedComplexity == -1) savedComplexity = Expansion.complexity(a) + Expansion.complexity(b)
		savedComplexity
	}
	def number = a * b
	def decomposition = f"$a * $b"
}

case class Sum(a: Int, b: Int) extends OpPair{
	require(a >= b)
	def complexity = Expansion.complexity(a) + Expansion.complexity(b)
	def number = a + b
	def decomposition = {
		val first = Expansion.expand(a).decomposition
		f"$first + $b"
	}
}

case class Empty() extends OpPair{
	def complexity = 1
	def number = 1
	def decomposition = "1"
}