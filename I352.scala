//Doesn't work yet
object challenge352I{
	// Cards and goal will be represented as a 4-D vector with each dimension representing a resource
	def main(args: Array[String])

	def format(input: String)

	def compute(cards: List[Vector[Int]], goal: Vector[Int]) : Bool = {
		def unpack(c: Vector[Int]) = c.zipWithIndex.filter(_._1 == 1).map(x => Vector.tabulate(4)(n => if (n == x._2) 1 else 0))
		match cards {
			case Nil => goal forall (_ == 0)
			case card :: rest => unpack(card) exists (path => compute(rest, goal - path))
		}
	}
}