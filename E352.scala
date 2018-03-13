object challenge352 {
	def main(args: Array[String]) = (args map(BigInt(_)) map(base62(_))) foreach println

	def base62(n: BigInt): String = {
		val alphabet = (('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')).toVector
		n match {
			case _ if n <= 0 => ""
			case _ => base62(n / 62) + alphabet((n % 62).toInt) 
		}
	}
}