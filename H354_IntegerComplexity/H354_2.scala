import scala.collection.mutable.Map

//12345678910111213 = 113 * (1 + 145 * (1 + 243 * (1 + 4 * (1 + 97 * 243 * (1 + 3 * 10962) ))) )
object H354{
	/* Save distances up to 10^9 for speed, 
	 then check for factors of 1 to 1000, 
	 can move down by 10 max
	*/
	var distances: Vector[Int] = Vector(0, 1, 2, 0, 1, 2, 2)

	def divisorPairs(n: Int) = for (d <- (2 to Math.sqrt(n).toInt) if (n % d == 0)) yield (n/d, d)
	def sumPairs(n: Int) = Vector((n - 1, 1))

	def integerComplexity(n: Int, safe: Boolean = false): Int = 
		if (distances.length > n) distances(n) + 3 * log3(n)
		else {
			val decomps = sumPairs(n) ++ divisorPairs(n)
			val complexities = decomps.map(x => integerComplexity(x._1) + integerComplexity(x._2))
			val complexity = complexities.min
			if (n < 200 && complexity == complexities.head && (complexity - 3 * log3(n)) < 10){
				goodNumbers = goodNumbers :+ n
			}
			if (!safe) distances = distances :+ (complexity - 3 * log3(n))
			complexity
		}

	def log3(n: Int): Int = if(n < 3) 0 else (1 + log3(n/3))
	def log3(n: BigInt): Int = if(n < 3) 0 else (1 + log3(n/3))

	def fastComplexity(n: Int): Int = fastComplexity(BigInt(n))

	def roughComplexity(n: BigInt): Int =  
		if (n < distances.length) distances(n.toInt) + 3 * log3(n)
		else roughComplexity(n/3) + (n % 3).toInt + 3
	def solve(n: BigInt): Int = solve(n, roughComplexity(n))
	def solve(n: BigInt, upperBound: Int, limit: Int = 12): Int = {
		var upperlimit = upperBound
		if (n < distances.length) distances(n.toInt) + 3 * log3(n)
		else {
			val toCheck = fastTriplets(n).toVector.sortBy(x => roughComplexity(x._1) + integerComplexity(x._2) + integerComplexity(x._3)).slice(0, limit)
			//val toCheck = fastTriplets(n).toVector.slice(0, limit)
			for ((a, b, c) <- toCheck if (3 * log3(a) + integerComplexity(b) + integerComplexity(c) < upperlimit)){
				val lag = integerComplexity(b) + integerComplexity(c)
				val potential = solve(a, upperlimit - lag, if (limit == 0) 0 else limit - 1) + lag
				upperlimit = 
					if (upperlimit > potential) potential
					else upperlimit
			}
			upperlimit
			//attempt1(n)
		}
	}

	def solveWithBreak(n: BigInt): (Int, List[BigInt]) = solveWithBreak(n, roughComplexity(n))
	def solveWithBreak(n: BigInt, upperBound: Int, limit: Int = 12): (Int, List[BigInt]) = {
		var returnV = (upperBound, Nil : List[BigInt])
		if (n < distances.length) (distances(n.toInt) + 3 * log3(n), BigInt(0) :: Nil)
		else {
			val toCheck = fastTriplets(n).toVector.sortBy(x => roughComplexity(x._1) + integerComplexity(x._2) + integerComplexity(x._3)).slice(0, limit)
			//val toCheck = fastTriplets(n).toVector.slice(0, limit)
			for ((a, b, c) <- toCheck if (3 * log3(a) + integerComplexity(b) + integerComplexity(c) < returnV._1)){
				val lag = integerComplexity(b) + integerComplexity(c)
				val potential = ((x : (Int, List[BigInt])) => (x._1 + lag, x._2))(solveWithBreak(a, returnV._1 - lag, if (limit == 0) 0 else limit - 1))
				returnV = 
					if (returnV._1 > potential._1){
						(potential._1, a:: potential._2)
					}
					else returnV
			}
			returnV
			//attempt1(n)
		}
	}

	val threshold = 2
	var goodNumbers: Vector[Int] = Vector(2, 3, 5)
	def fastTriplets(n: BigInt) = for (b <- goodNumbers if (n % b < threshold)) yield((n/b, b.toInt, (n % b).toInt))

	def fastComplexity(n: BigInt): Int = fastComplexity(n, 6 * log3(n))
	def fastComplexity(n: BigInt, upperBound: Int, depth: Int = 0): Int = {
		var upperlimit = upperBound
		if (n < distances.length) distances(n.toInt) + 3 * log3(n)
		else {
			for ((a, b, c) <- fastTriplets(n) if (3 * log3(a) + integerComplexity(b) + integerComplexity(c) < upperlimit)){
				val lag = integerComplexity(b) + integerComplexity(c)
				val potential = fastComplexity(a, upperlimit - lag, depth + 1) + lag
				upperlimit = Vector(upperlimit, potential).min
			}
			upperlimit
			//attempt1(n)
		}
	}

	def attempt1(n: BigInt){
		val trips = fastTriplets(n)
		//val l = trips.length
		//println(f"$n has decomp of size $l")
		trips.map(x => fastComplexity(x._1) + integerComplexity(x._2.toInt) + integerComplexity(x._3.toInt)).min
	}

	def main(args: Array[String]) {
		val data = time {(1 to 1000000) map(integerComplexity(_))}
		//goodNumbers = (2 to 1000).toVector.filter(x => distances(x) < 3)
		println(goodNumbers.length)
		//println(goodNumbers)
		//println(time{integerComplexity(1000205, true)})
		//println(time{fastComplexity(1000205)})
		//println((1000000 to 1000100) map(fastComplexity(_)))
		//println((1000000 to 1000100) map(integerComplexity(_)))
		println(time{solveWithBreak(BigInt("12345678910111213"))}) // At most 78910111213 - 77
	}

	def time[R](block: => R): R = {  
	    val t0 = System.nanoTime()
	    val result = block    // call-by-name
	    val t1 = System.nanoTime()
	    println("Elapsed time: " + (t1 - t0)/1000000000 + "s")
	    result
	}

}