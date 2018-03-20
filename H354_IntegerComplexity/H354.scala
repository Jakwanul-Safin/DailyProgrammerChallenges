import scala.collection.mutable.Map
object H354{
	val rememberedValues: Map[Int, OpPair] = Map(1 -> Empty(), 2 -> Sum(1, 1), 3 -> Sum(2, 1))

	def divisorPairs(n: Int) = for (d <- (2 to Math.sqrt(n).toInt) if (n % d == 0)) yield Prod(n/d, d)
	def sumPairs(n: Int) = for (s <- (1 to 1)) yield Sum(n - s, s)

	var total = 0
	var prods = 0
	var sums = 0
	var specials = 0

	def integerComplexity(n: Int): OpPair = rememberedValues.get(n) match {
		case Some(x) => x
		case None => {
			total += 1
			val decomps = sumPairs(n) ++ divisorPairs(n)
			val complexity = decomps.minBy(_.complexity)
			complexity match{
				case Sum(_,_) => sums+=1
				case Prod(a, b) => {
					prods+=1
					if (b < 10) specials += 1
					else {
						val (aAcc, bAcc, cAcc) = (integerComplexity(a).accuracy, integerComplexity(b).accuracy, complexity.accuracy)
						println(f"$n with $complexity and accuracy $aAcc + $bAcc = $cAcc")
					}
				}
				case _ => 0
			}
			rememberedValues.put(n, complexity)
			complexity
		}
	}

	def main(args: Array[String]) {
		val data = (1 to 32887) map(integerComplexity(_)) filter(
			x => x match {
				case Sum(_, 1) => true
				case _ => true
			}
		) map(x => (x.number, x, x.decomposition, x.complexity))
		//data foreach println
		//println(f"The total is $total with $prods products, $sums sums and $specials of a specified type")
		val love = Vector(113, 145, 243, 4, 97, 243, 3, 10962).map(integerComplexity(_).complexity).sum + 5
		println(love)
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
			if (savedComplexity == -1) savedComplexity = integerComplexity(a).complexity + integerComplexity(b).complexity
			savedComplexity
		}
		def number = a * b
		def decomposition = f"$a * $b"
	}
	case class Sum(a: Int, b: Int) extends OpPair{
		require(a >= b)
		def complexity = integerComplexity(a).complexity + integerComplexity(b).complexity
		def number = a + b
		def decomposition = {
			val first = integerComplexity(a).decomposition
			f"$first + $b"
		}
	}

	case class Empty() extends OpPair{
		def complexity = 1
		def number = 1
		def decomposition = "1"
	}

}