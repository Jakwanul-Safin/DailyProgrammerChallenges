package o

object StoredValues{
	/* Distance of a number is the different between its complexity and the 3log3(n) bound.
	 */
	var distances: Vector[Int] = Vector(0, 1, 2, 0, 1, 2, 2)
	def divisorPairs(n: Int) = (2 to Math.sqrt(n).toInt).view.filter(n % _ == 0).map(d => (n/d, d))
	def sumPair(n: Int) = (n - 1, 1)
	def heuristic(n: Int) = 3 * log3(n)

	def buildTo(n: Int, verbose: Boolean = false){
		for (i <- (distances.length to n)){
			val decomps = sumPair(i) +: divisorPairs(i)
			val complexity = decomps.map(x => rawComplexity(x._1) + rawComplexity(x._2)).min
			distances = distances :+ (complexity - 3 * log3(i))
			if (verbose && (i % 100000 == 0)) println(f"Calculated values up to $i")
		}
	}

	private def rawComplexity(n: Int): Int = distances(n) + 3 * log3(n)
	def log3(n: Int): Int = if (n < 3) 0 else (1 + log3(n/3))

	def complexity(n: Int): Option[Int] = if (distances.length > n) Some(rawComplexity(n)) else None
}