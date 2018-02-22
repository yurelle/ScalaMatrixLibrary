import scala.collection.mutable.ListBuffer

object intro extends App {
	println("Hello, World!")

	val nums = ((0 to 10 by 2).toList :+ 16) ::: (50.to(60).by(5)).toList

	val otherNums = nums.drop(3)
											.withFilter( _%2 == 0) //.filter generates a new list; .withFilter just restricts visibility of the existing list
											.map(v => v*10)
									    .map(v => {
											  "\nMy Num is: " + v
										  })

	println("\nNums:\n---\n"+nums)
	println("\nOtherNums:\n---\n"+otherNums)

	val range = (-5 to -1 by 1) :+ 123
	val inverseRange = (10 to 1 by -2).map(_/2) :+ "\"Fire!\""
	val indexRange = (0 until 5).toList

	println("\nRange:\n---\n"+range)
	println("\nInverseRange:\n---\n"+inverseRange)
	println("\nIndexRange:\n---\n"+indexRange)

	for (i <- 1 to 5)
		println("for["+i+"]")

	var buf = new ListBuffer[String]
	buf += "Hello"
	buf += " "
	buf += "World"
	buf += "!"
	println(buf)

	buf.zipWithIndex.foreach {
		case(x,i) => {
			println(s"x: '$x'\t\ti: $i")
		}
	}

	for (x <- (0 until 3); y <- (2 to 0 by -1)) {
		println(s"[$x, $y] = ${x+y}")
	}
}
