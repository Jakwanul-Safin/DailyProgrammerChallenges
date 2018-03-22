package o
import scala.collection.mutable.PriorityQueue

object FastComplexity{
	val primeComplexities = Expansion.primeComplexities(3000)
	val storage = 100000

	def roughComplexity(n: BigInt, corseness: Int = 1000) = {
		var pq = PriorityQueue(new Node(n))(Ordering.by((x: Node) => -x.estimate))
		var best = 6 * Expansion.log3(n)
		while (!pq.isEmpty){
			val next = pq.dequeue.softSplit
			for (pot <- next){
				if (pot.n < storage) {
					if (pot.complexity < best) {
						best = pot.complexity
					}
				}
				else if (pot.heuristic < best) {
					pq.enqueue(pot)
				}
			}
			pq = pq take corseness
		}
		best
	}

	def standardComplexity(n: BigInt, corseness: Int = 100, guess: Int = -1) = {
		var pq = PriorityQueue(new Node(n))(Ordering.by((x: Node) => -x.estimate))
		var best = if (guess == -1) 6 * Expansion.log3(n) else guess
		while (!pq.isEmpty){
			//println(pq.take(5).map(_.lineage))
			val next = pq.dequeue.medSplit
			for (pot <- next){
				if (pot.n < storage) {
					if (pot.complexity < best) {
						best = pot.complexity
					}
				}
				else if (pot.heuristic < best) {
					pq.enqueue(pot)
				}
			}
			pq = pq take corseness
		}
		best
	}

	def intelligentComplexity(n: BigInt) = {
		if (n > BigInt("83719383651")) roughComplexity(n, 1000)
		else standardComplexity(n, 1000)
	}

	def crumple(layer: IndexedSeq[Node]) = {
		val candidates = layer.map(_.medSplit).reduce(_ ++ _).toVector
		candidates.map(node => (node, 100 * (intelligentComplexity(node.n) + node.lag) - node.lag )).sortBy(_._2).map(_._1).distinct take 25
	}

	def solve(start: IndexedSeq[Node]) = {
		var lst = start
		var best: Option[Node] = None
		var n = 0
		while(!lst.isEmpty){
			println(n + " with list " + lst)
			n += 1
			lst = crumple(lst)
			for (x <- lst.filter(_.n < storage)){
				best = best match {
					case None => Some(x)
					case Some(node) if (node.complexity > x.complexity) => Some(x)
					case _ => best
				}
			}
			lst = lst.filter(_.n > storage)
		}
		best
	}

	def complexity(n: BigInt, corseness: Int = 100) = {
		var pq = PriorityQueue(new Node(n))(Ordering.by((x: Node) => -x.estimate))
		var best = roughComplexity(n, 10)
		var printFirst = 100
		while (!pq.isEmpty){
			if (printFirst > 0){
				printFirst -= 1
				println(pq.head.lineage)
			}
			val next = pq.dequeue.medSplit
			for (pot <- next){
				if (pot.n < storage) {
					if (pot.complexity < best) best = pot.complexity
				}
				else if (pot.heuristic < best) {
					pot.estimateSave = roughComplexity(pot.n, 1000) + pot.lag
					if (pot.estimate < best) best = pot.estimate
					pq.enqueue(pot)
				}
				pq.filter(_.heuristic < best)
				pq = pq take corseness
			}
		}
		best
	}

	def main(args: Array[String]){
		Expansion.complexity(storage)
		println("Finished Storing Values")
		println("Primes stored to length " + primeComplexities.length)
		//println(time{roughComplexity(BigInt("12345678910111213"), 1000)})
		println(standardComplexity(BigInt("9302153739"), 1000))
		println(roughComplexity(BigInt("9302153739"), 1000))
		println(roughComplexity(BigInt("9302153739"), 500))
		println(time{standardComplexity(BigInt("9302153739"), 500)})
		println(time{
			solve(Vector(new Node(BigInt("1234567891011121314")))) match{
					case Some(n) => n.complexity
					case _ => "Error"
				}
			}
		)
		println(time{
			solve(Vector(new Node(BigInt("123456789101112131415")))) match{
					case Some(n) => n.complexity
					case _ => "Error"
				}
			}
		)
		println(time{
			solve(Vector(new Node(BigInt("12345678910111213141516")))) match{
					case Some(n) => n.complexity
					case _ => "Error"
				}
			}
		)
	}

	def time[R](block: => R): R = {  
	    val t0 = System.nanoTime()
	    val result = block 
	    val t1 = System.nanoTime()
	    println("Elapsed time: " + (t1 - t0)/1000000000 + "s")
	    result
	}
}

class Node(val n: BigInt, prev: Option[Node] = None, val lag: Int = 0) {
	var heuristicSave = -1
	var estimateSave = -1
	def estimate = {
		if (estimateSave == -1) estimateSave = heuristic
		estimateSave
	}
	def heuristic = {
		if (heuristicSave == -1) heuristicSave = 3 * log3(n) + 1 + lag
		heuristicSave
	}
	def softSplit = Vector(crack(2), crack(3), crack(5))
	def medSplit = FastComplexity.primeComplexities.map(x => restrictiveCrack(x)).flatten

	def crack(d: Int) = new Node(n/d, Some(this), lag + (n % d).toInt + Expansion.complexity(d))
	def restrictiveCrack(d: Int) = if(n % d > 1) None else Some(crack(d))
	def complexity = Expansion.complexity(n.toInt) + lag

	def lineage = prev match {
		case None => "Root!"
		case Some(parent) =>  n + " from " + parent.n + " division by " + (parent.n/n)
	}


	override def hashCode = (n % 1000).toInt + lag
	override def equals(that: Any) = that match{
		case that: Node => ((that.n == n) && (that.lag == lag))
		case _ => false
	}
	override def toString = f"$n with lag $lag"
	private def log3(n: BigInt): Int = if (n < 3) 0 else (1 + log3(n/3))
	private def log10(n: BigInt): Int = if (n < 10) 0 else (1 + log3(n/10))
}