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
	//See: https://pavelfatin.com/scala-collections-tips-and-tricks/#sequences-rewriting
	//See: https://medium.com/@hussachai/scalas-immutable-collections-can-be-slow-as-a-snail-da6fc24bc688
	val otherNums = nums.view
											.drop(3)
											.withFilter( _%2 == 0) //.filter generates a new list; .withFilter just restricts visibility of the existing list (.view prevents .filter from doing this, but just to be explicit for this example)
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
			println(s"x: '$x'\t\ti: $i")//See: https://docs.scala-lang.org/overviews/core/string-interpolation.html
		}
	}

	for (x <- (0 until 3); y <- (2 to 0 by -1)) {
		println(s"[$x, $y] = ${x+y}")
	}

	val a = Array(1 to 9).grouped(3)
	val aa = a.toArray
	val aT = aa.transpose

	println("\na:\n---\n"+a)
	println("\naa:\n---\n"+aa)
	println("\naT:\n---\n"+aT)

	//Basic Tuple Access (one based NOT zero based)
	val tuple = ('A', 'B', 'C')
	println(s"\nBasic Tuple:\n---\n" +
		s"Tuple[0] (._1): ${tuple._1}\n" +
		s"Tuple[1] (._2): ${tuple._2}\n" +
		s"Tuple[2] (._3): ${tuple._3}\n"
	)

	//Cool Tuple Extraction Trick
	//
	//You can split the values out of a tuple to separate variables in a single step. For example, if a function returned
	//a tuple, but you want them as separate vals, you can do this.
	def tupleFunc():(Int,Int) ={
		return (1,2)
	}
	val (one,two) = tupleFunc()
	println(s"\nCool Tuple Trick:\n---\nOne = $one\nTwo = $two")

	//Store case partial function
	//
	//See: https://damieng.com/blog/2014/12/11/sequence-averages-in-scala
	//
	//val average = seq.sum / seq.length
	//This has a few problems:
	//  1. Visiting a sequence twice can be inefficient
	//  2. Sum can overflow as it is the same type as the sequence
	//  3. Applied to an integer without casting it returns an integer average
	//
	//See: http://blog.bruchez.name/2011/10/scala-partial-functions-without-phd.html
	//See: https://alvinalexander.com/scala/fp-book-diffs-val-def-scala-functions
	//See: https://stackoverflow.com/a/7764889/7206367
	val s1 = Array[Long]  (1,2,3,4,5)
	val s2 = Array[Double](6,7,8,9,10)
	val length = s1.length
	val reduceStats: PartialFunction[(IndexedSeq[Long], IndexedSeq[Double]), (Long, Double)] = {
		case (e1:IndexedSeq[Long], e2:IndexedSeq[Double]) => (
			e1.reduce[Long]  (_+_),
			e2.reduce[Double](_+_) / length
		)
	}
	println(s"\nAnonymous Case Function:\n---\n$reduceStats")
	println(s"\nS1:\n---\n$s1")
	println(s"\nS2:\n---\n$s2")

	val (s1_t, s2_t) = reduceStats(s1,s2)
	println(s"\n\n---\nS1_T:$s1_t\tS2_T:$s2_t")
}
