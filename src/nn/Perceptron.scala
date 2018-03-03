package nn

import scala.util.Random

//For solving linearly separable problems only
class Perceptron(val numInputs:Int, val learningRate: Double) {
	val rand = Random
	var weights = Array.fill(numInputs){rand.nextDouble()}.toIndexedSeq //0.0-1.0

	//Bias value is always 1, so the multiplication by it is unnecessary; just biasWeight is fine.
	var biasWeight = rand.nextDouble()

	def train(inputData:IndexedSeq[Double], target:Double) = {
		//Generate prediction
		val guess = feedForward(inputData)

		//Calculate error rate
		val error = target - guess

		//Adjust weights
		val scaledAdjustmentFactor = error * learningRate
//		weights = weights.map(w => w + (w * scaledAdjustmentFactor))
		weights = (weights zip inputData).map{
			case(w,i) => w + (i * scaledAdjustmentFactor)
		}

		//Adjust bias weight
		biasWeight += scaledAdjustmentFactor
	}

	def feedForward(inputs:IndexedSeq[Double]): Double = {
		require(inputs.length == numInputs, s"inputs.length '${inputs.length}' is not equal to numInputs '$numInputs'")

		//Apply Weights
		val inputSum = (inputs zip weights).map{
			case(input, weight) => input * weight
		}.reduce(_+_)

		//Apply Bias
		val finalSum = inputSum + biasWeight

		//Calculate Activation
		return activate_binary(finalSum)
	}

	//See: https://en.wikipedia.org/wiki/Activation_function#Comparison_of_activation_functions
	def activate_continuous(sum:Double): Double = {
		//ArcTan has a range of values between +- Pi/2. So, we divide by it
		//to get the percentage of activation [-1 -> +1]
		return Math.atan(sum) / (Math.PI/2)
	}

	//Using this activation function the only possible values are (-1 & 1), and thus
	//the only possible error values are (-2, 0, & 2)
	def activate_binary(sum:Double): Double = {
		//Scala doesn't have a ternary operator
		//See: https://alvinalexander.com/scala/scala-ternary-operator-syntax
		return if (sum > 0) 1 else -1
	}

}
