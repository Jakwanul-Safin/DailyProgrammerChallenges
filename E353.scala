import scala.io.Source
// Work in progress. Currently a naive implementation

object c353{
	def main(args: Array[String]){
		println(center(args: _*))
	}

	def hammingDist(s1: String, s2: String) = s1 zip s2 count(x => x._1 == x._2)
	def pairWiseDist(s: String, words: String*) = words map(hammingDist(s, _))
	def center(words: String*) = words.maxBy(pairWiseDist(_, words:_*).sum)
}