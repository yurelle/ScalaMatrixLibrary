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


	/////////////////////////////////////////////////////////

	val beforeInit = System.currentTimeMillis();
	val dim = 1000

	//Arrays don't fully implement collections API, and break the lazy-view chain. Convert to a full-collection type.
	//See: https://stackoverflow.com/questions/40517391/scala-view-force-is-not-a-member-of-seq
	val a1_2D:IndexedSeq[IndexedSeq[Double]] = Array.ofDim[Double](dim,dim).map(_.toIndexedSeq)
	val a2_2D:IndexedSeq[IndexedSeq[Double]] = Array.ofDim[Double](dim,dim).map(_.toIndexedSeq)
	val a1_1D = Array.ofDim[Double](dim*dim)
	val a2_1D = Array.ofDim[Double](dim*dim)
	val afterInit = System.currentTimeMillis();

	println(s"\ninit took ${afterInit-beforeInit}ms")

	println(s"\na1.flatten:\n---\n"+a1_2D.flatten)

	def bench(f:() => Unit, name:String): (Long, Double) = {
		printf(s"Starting task: $name...")

		val numIterations = 10;
		val beforeTask = System.currentTimeMillis();

		for (_ <- 1 to  numIterations) {
			f()
		}

		val afterTask = System.currentTimeMillis();
		val total = afterTask-beforeTask;
		val perRun = total/numIterations
		println(s"Done! Took ${total}ms\t\t[${perRun}ms per run]")

		return (total, perRun)
	}

	def t1_slow():IndexedSeq[IndexedSeq[Double]] = {
		//Super slow
		return (a1_2D.view.flatten.toList zip a2_2D.view.flatten.toList).map {
			case (e1, e2) => e1 + e2
		}.view.toIndexedSeq.grouped(dim).toIndexedSeq
	}

	def t1_fast():IndexedSeq[IndexedSeq[Double]] = {
		//Really Fast
		return (a1_2D zip a2_2D).map({case (a1,a2) => (a1 zip a2).map{case (e1, e2) => e1 + e2}.toIndexedSeq})
	}

	//Identical to t1_fast, just reorganized to make it easier to follow
	def t1_fast_organized():IndexedSeq[IndexedSeq[Double]] = {
		//Really Fast
		//See: http://blog.bruchez.name/2011/10/scala-partial-functions-without-phd.html
		//See: https://alvinalexander.com/scala/fp-book-diffs-val-def-scala-functions
		val mapCols: PartialFunction[(Double,Double),Double] = {
			case (e1, e2) => e1 + e2
		}
		val mapRows: PartialFunction[(IndexedSeq[Double],IndexedSeq[Double]),IndexedSeq[Double]] = {
			case (a1,a2) => (a1 zip a2).map(mapCols).toIndexedSeq
		}
		return (a1_2D zip a2_2D).map(mapRows);
	}

	def t2():IndexedSeq[Double] = {
		return (a1_1D zip a2_1D).map {
			case (e1, e2) => e1 + e2
		}
	}

	//--Do Benching
	val t1_results = ListBuffer[(Long, Double)]()
	val t2_results = ListBuffer[(Long, Double)]()

	val numWarmups = 4;
	println(s"\nRunning Warmups [$numWarmups]")
	for(_ <- 1 to numWarmups) {
		bench(t1_fast, "2D")
		bench(t2, "1D")
	}
	println("Warmups Complete!")

	val batchSize = 10
	println(s"\nStarting tests [$batchSize]")
	for(_ <- 1 to batchSize) {
		t1_results.append(bench(t1_fast, "2D"))
		t2_results.append(bench(t2, "1D"))
	}
	println("All tests complete!")

	printf("\nCalculating stats...")
	def sumTuple(e1:(Long, Double), e2:(Long, Double)): (Long,Double) = {
		return (e1._1 + e2._1,
						e1._2 + e2._2)
	}

	//See: https://stackoverflow.com/a/7764889/7206367
	val (t1_results_totals, t1_results_avgs) = t1_results.unzip
	val (t2_results_totals, t2_results_avgs) = t2_results.unzip

	//See: https://damieng.com/blog/2014/12/11/sequence-averages-in-scala
	//
	//val average = seq.sum / seq.length
	//This has a few problems:
	//  1. Visiting a sequence twice can be inefficient
	//  2. Sum can overflow as it is the same type as the sequence
	//  3. Applied to an integer without casting it returns an integer average
	val t1_stats = (
		t1_results_totals.reduce[Long](_+_),
		t1_results_avgs.reduce[Double](_+_) / batchSize
	)
	val t2_stats = (
		t2_results_totals.reduce[Long](_+_),
		t2_results_avgs.reduce[Double](_+_) / batchSize
	)

//	val t1_totals = t1_results.reduce[(Long, Double)](sumTuple)
//	val t2_totals = t2_results.reduce[(Long, Double)](sumTuple)
	println("Done!")
	println(s"\nResults:\n---")
	println(s"T1:\t\tAvg [${t1_stats._2}ms]\tTotal [${t1_stats._1}]")
	println(s"T2:\t\tAvg [${t2_stats._2}ms]\tTotal [${t2_stats._1}]")

}
