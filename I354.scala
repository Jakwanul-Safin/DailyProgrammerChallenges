import scala.collection.mutable.Map
object I354{
	val rememberedValues: Map[Int, Int] = Map(1 -> 1, 2 -> 2, 3 -> 3)
	def divisorPairs(n: Int) = (Math.sqrt(n).toInt until 1 by -1) filter(n % _ == 0) map(d => (n/d, d))
	def sumPairs(n: Int) = (n/2 to 1 by -1) map(s => (n - s, s))
	def integerComplexity(n: Int): Int = rememberedValues.get(n) match {
		case Some(x) => x
		case None => {
			def tupleSum(p: (Int, Int)) = integerComplexity(p._1) + integerComplexity(p._2)
			val complexity = ((divisorPairs(n) ++ sumPairs(n)) map(tupleSum(_))).min
			rememberedValues.put(n, complexity)
			complexity
		}
	}

	def main(args: Array[String]) {
		println(((1 to 100) map(integerComplexity(_))).sum)
	}
}

object NumberTheory{
	def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
	def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

	def factor(n: Int, init: Int = 2): Int = {
		if (n % 2 == 0) 2
		else {
			def g(x: Int) = ( (x * x + 1) % n )
			def loop(x: Int, y: Int, d: Int): Option[Int] = 
				if (d == 1) {
					val (w, z) = (g(x), g(g(y)))
					loop(w, z, gcd(Math.abs(w - z), n))
				}
				else 
					if (d == n) None
					else Some(d)
			loop(init, init, 1) match {
				case None => {
					if (isPrime(n)) n
					else factor(n, init + 1)
				}
				case Some(d) => d
			}
		}
	}

	def isPrime(n: Int) =
		if (n <= 1) false
		else if(n <= 3) true
		else if((n % 2 == 0) || (n % 3 == 0)) false
		else {
			!(5 to Math.sqrt(n).toInt by 6).exists(i => (n % i == 0) || (n % (i+2) == 0))
		}

	def factorization(n: Int): Vector[Int] = n match {
		case 1 => Vector()
		case _ => {
			val d = factor(n)
			if (d == n) d +: factorization(n/d)
			else factorization(d) ++ factorization(n/d)
		}
	}

	def main(args: Array[String]){
		println(factorization(132))
	}
}