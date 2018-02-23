import scala.collection.mutable.ListBuffer

object intro extends App {
	println("Hello, World!")

	val nums = ((0 to 10 by 2).toList :+ 16) ::: (50.to(60).by(5)).toList

	//View creates a lazily-evaluated translation map to the original data which then accumulates all modifications following it until it is "forced" (i.e. list.view.mapFilterEtc.force)
	//Without the view (i.e. acting directly on a collection), causes a new complete collection to be build after EVERY function (map, filter, etc.)
	//A view is equivalent to a stream, except that a stream caches calculated values, where as a view does not. Thus a stream is more efficient if you access values more than once, but
	//A view is more efficient (smaller memory footprint, since no caching), if you're only going through the values once (like when forcing it).
	//A scala view has also been compared to a SQL Database view (i.e. a non-cached translation matrix, pointing to the original data). The view can be access like this, calculating each
	//value upon request, or it can be "forced", causing it to run through all elements, doing the translation for each, and generate a final resulting collection from them.
	val otherNums = nums.view
											.drop(3)
											.withFilter( _%2 == 0) //.filter generates a new list; .withFilter just restricts visibility of the existing list
											.map(v => v*10)
									    .map(v => {
											  "\nMy Num is: " + v
										  })
									  	.force

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
