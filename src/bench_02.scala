import nn.Benchmarker

import scala.collection.mutable.ListBuffer
import scala.util.Random

object bench_02 extends App {
	val rand = new Random(1) //set seed for consistent tests

	val length = 5000000
	val inputs = Array.fill(length){rand.nextDouble() * 100}
	val weights = Array.fill(length){rand.nextDouble()}

	println(s"\nInputs:\n---\n"+inputs)
	println(s"\nWeights:\n---\n"+weights)

	def reduce(): Double = {
		return (inputs zip weights).map{
			case(input, weight) => input * weight
		}.reduce(_+_)
	}

	def reduce_left(): Double = {
		return (inputs zip weights).map{
			case(input, weight) => input * weight
		}.reduceLeft(_+_)
	}

	def reduce_right(): Double = {
		return (inputs zip weights).map{
			case(input, weight) => input * weight
		}.reduceRight(_+_)
	}

	def reduce_par(): Double = {
		return (inputs zip weights).par.map{
			case(input, weight) => input * weight
		}.reduce(_+_)
	}

	def fold(): Double = {
		return ((inputs zip weights):Array[(Double,Double)]).map{
			case(input, weight) => input * weight
		}.fold[Double](0)(_+_)
	}

	def fold_left(): Double = {
		return ((inputs zip weights):Array[(Double,Double)]).map{
			case(input, weight) => input * weight
		}.foldLeft[Double](0)(_+_)
	}

	def fold_right(): Double = {
		return ((inputs zip weights):Array[(Double,Double)]).map{
			case(input, weight) => input * weight
		}.foldRight[Double](0)(_+_)
	}

	def fold_par(): Double = {
		return (inputs zip weights).par.map{
			case(input, weight) => input * weight
		}.fold[Double](0)(_+_)
	}

	//--Do Benching
	val benchmarker = new Benchmarker(NUM_BENCH_ITERATIONS = 10,
																		NUM_WARMUPS = 4,
																		BENCH_BATCH_SIZE = 10)

	val funcList = IndexedSeq[(() => Unit, String)](
		(reduce,      "Reduce      "),
		(reduce_par,  "Reduce_par  "),
		(reduce_left, "Reduce_left "),
		(reduce_right,"Reduce_right"),
		(fold,        "Fold        "),
		(fold_par,    "Fold_par    "),
		(fold_left,   "Fold_left   "),
		(fold_right,  "Fold_right  ")
	)

	benchmarker.warmup(funcList)
	benchmarker.profileBatch(funcList)//IndexedSeq[IndexedSeq[(Long,Double)]]
}
