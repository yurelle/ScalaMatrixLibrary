package nn

import scala.collection.mutable.ListBuffer

object bench_01 extends App {
	val beforeInit = System.currentTimeMillis();
	val dim = 1000

	//Arrays don't fully implement collections API, and break the lazy-view chain. Convert to a full-collection type.
	//See: https://stackoverflow.com/questions/40517391/scala-view-force-is-not-a-member-of-seq
	val a1_1D = Array.ofDim[Double](dim*dim)
	val a2_1D = Array.ofDim[Double](dim*dim)
	val a1_2D:IndexedSeq[IndexedSeq[Double]] = Array.ofDim[Double](dim,dim).map(_.toIndexedSeq)
	val a2_2D:IndexedSeq[IndexedSeq[Double]] = Array.ofDim[Double](dim,dim).map(_.toIndexedSeq)
	val afterInit = System.currentTimeMillis();

	println(s"\ninit took ${afterInit-beforeInit}ms")

	println(s"\na1.flatten:\n---\n"+a1_2D.flatten)


	def t1_slow():IndexedSeq[Double] = {
		return (a1_1D zip a2_1D).map {
			case (e1, e2) => e1 + e2
		}
	}

	def t1_fast():IndexedSeq[Double] = {
		return (a1_1D zip a2_1D).par.map {
			case (e1, e2) => e1 + e2
		}.toIndexedSeq
	}

	def t2_slow():IndexedSeq[IndexedSeq[Double]] = {
		//Super slow
		return (a1_2D.view.flatten.toList zip a2_2D.view.flatten.toList).map {
			case (e1, e2) => e1 + e2
		}.view.toIndexedSeq.grouped(dim).toIndexedSeq
	}

	def t2_fast():IndexedSeq[IndexedSeq[Double]] = {
		//Really Fast
		return (a1_2D zip a2_2D).map({case (a1,a2) => (a1 zip a2).map{case (e1, e2) => e1 + e2}.toIndexedSeq})
	}

	//Identical to t2_fast, just reorganized to make it easier to follow
	def t2_fast_organized():IndexedSeq[IndexedSeq[Double]] = {
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

	//--Do Benching
	val benchmarker = new Benchmarker(NUM_BENCH_ITERATIONS = 10, NUM_WARMUPS = 4, BENCH_BATCH_SIZE = 10)

	val funcList = IndexedSeq[(() => Unit, String)](
		(t1_slow, "1D_slow"),
		(t1_fast, "1D_fast"),
		(t2_slow, "2D_slow"),
		(t2_fast, "2D_fast")
	)

	benchmarker.warmup(funcList)
	benchmarker.profileBatch(funcList)
}
