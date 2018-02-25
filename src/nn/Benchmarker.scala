package nn

import scala.collection.mutable.ListBuffer

class Benchmarker(NUM_BENCH_ITERATIONS: Int, NUM_WARMUPS: Int, BENCH_BATCH_SIZE: Int) {
	def bench(f:() => Unit, name:String): (String, Long, Double) = {
		printf(s"Starting task: $name...")

		val beforeTask = System.currentTimeMillis();

		for (_ <- 1 to  NUM_BENCH_ITERATIONS) {
			f()
		}

		val afterTask = System.currentTimeMillis();
		val total = afterTask-beforeTask;
		val perRun = total/NUM_BENCH_ITERATIONS
		println(s"Done! Took ${total}ms\t\t[${perRun}ms per run]")

		return (name, total, perRun)
	}

	def warmup(funcList: IndexedSeq[(() => Unit, String)]): IndexedSeq[IndexedSeq[(String, Long, Double)]] = {
		println(s"\nRunning Warmups [$NUM_WARMUPS]")
		val stats = _exeBatch(funcList, NUM_WARMUPS)
		println("Warmups Complete!")

		return stats;
	}

	def runBatch(funcList: IndexedSeq[(() => Unit, String)]): IndexedSeq[IndexedSeq[(String, Long, Double)]] = {
		println(s"\nStarting tests [$BENCH_BATCH_SIZE]")
		val stats = _exeBatch(funcList, BENCH_BATCH_SIZE)
		println("All tests complete!")

		return stats;
	}

	def _exeBatch(funcList: IndexedSeq[(() => Unit, String)], batchSize: Int): IndexedSeq[IndexedSeq[(String, Long, Double)]] = {
		val results = ListBuffer[IndexedSeq[(String, Long,Double)]]()
		for((f,name) <- funcList) {
			val funcStats = ListBuffer[(String, Long, Double)]()
			for (_ <- 1 to batchSize) {
				funcStats.append(bench(f, name))
			}
			results.append(funcStats.toIndexedSeq)
		}
		return results.toIndexedSeq
	}

	def profileBatch(funcList: IndexedSeq[(() => Unit, String)]) = {
		val stats = reduceStatList(runBatch(funcList))

		println(s"\nResults:\n---")
		stats.foreach {
			case (name, total, avg) => {
				println(s"${name}:\t\tAvg [${avg}ms]\tTotal [${total}ms]")
			}
		}
	}

	def reduceStatList(statLists:IndexedSeq[IndexedSeq[(String, Long,Double)]]): IndexedSeq[(String, Long, Double)] = {
		statLists.map(
			(funcStats) => {
				funcStats.unzip3
			}
		).map {
			case (name:IndexedSeq[String], totals:IndexedSeq[Long], avgs:IndexedSeq[Double]) => (
				name(0),
				totals.reduce[Long](_+_),
				avgs.reduce[Double](_+_) / BENCH_BATCH_SIZE
			)
		}
	}


}
