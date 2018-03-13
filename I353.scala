import scala.io.Source

object Solver{
	val seperator = "----"

	def main(args: Array[String]){
		val filename = args(0)
		val inputs = splitInput(Source.fromFile(filename).getLines.toList, seperator)
		def writeSoln(input: List[String]) {
			val quantity :: pancakesString :: Nil = input
			val pancakes = (pancakesString split " " map(_.toInt)).toVector
			require(pancakes.length == quantity.toInt)
			println {
				val sol = solve(pancakes)
				sol.length.toString ++ " flips: " ++ (sol map(_.mkString(" "))).mkString(" -> ")
			}
		}

		inputs foreach writeSoln
	}

	def solve(seq: Vector[Int]): List[Vector[Int]] = {
		val max = if (!seq.isEmpty) seq.max else 0
		seq match {
			case Vector() => seq :: Nil
			case in :+ la if (la == max) => solve(in) map(_ :+ la)
			case h +: t if (h == max) => seq :: (solve(t.reverse) map(_ :+ h))
			case _ => seq :: solve(swap(seq, seq.indexOf(max)))
		}
	}

	def swap(seq: Vector[Int], pos: Int) = seq.slice(0, pos + 1).reverse ++ seq.slice(pos + 1, seq.length)

	def splitInput[A](seq: List[A], regex: A): List[List[A]] = seq match {
		case Nil => Nil
		case e :: Nil => (e :: Nil) :: Nil
		case h :: t if (h == regex) => Nil :: splitInput(t, regex)
		case _ => {
			val h :: t = splitInput(seq.tail, regex)
			(seq.head :: h) :: t
		}
	}
}


