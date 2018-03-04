package nn

import nn.matrix.{Matrix, Vector}

import scala.collection.mutable.ListBuffer
import scala.util.Random


/**
	* Legend:
	* -------
	* N: Node
	* W: Weight
	* B: Bias
	* I: Input Data
	* S: Sum
	* O: Output Data
	* T: Target Value (The expected output of training data)
	* E: Error (The difference between the output & the target)
	* LR: Learning Rate
	* **: Matrix Multiplication
	* fn_a(): Activation Function
	*
	*
	*                   [B1]
	*                     \
	*                      \
	* (N1) -> [I1] --W1--> (N3) -> [S1] --fn_a(S1)--> [O1]
	*             W2\  /
	*                \/
	*                /\
	*             W3/  \
	* (N2) -> [I2] --W4--> (N4) -> [S2] --fn_a(S2)--> [O2]
	*                      /
	*                     /
	*                   [B2]
	*
	*
	* Calculation Matrix:
	* -------------------
	*
	* N3 Weights Row --> | W1   W3 |    | I1 |   | B1 |   | S1 | <-- N3 Output
	*                    |         | ** |    | + |    | = |    |                 => Activation Function
	* N4 Weights Row --> | W2   W4 |    | I2 |   | B2 |   | S2 | <-- N4 Output
	*                      ^    ^
	*                      |    |
	*                     N1    N2
	*                 Source    Source
	*                 Column    Column
	*
	* Because matrix multiplication is of the form:
	*
	*     A.row * B.col = C.cell
	*
	* and, since the input matrix is a 1D vector, each row in the weights matrix will
	* produce a corresponding Single-Celled-Row in the result vector. And since matrix
	* multiplication is Fixed Direction specific, the result matrix/vector and input
	* vector, dictate the order/structure of the weights matrix (i.e. perpendicular
	* to the input vector).
	*
	* Activation:
	* -----------
	*
	* The only other calculation which needs to be done to finalize the output is to
	* pass the sum value through an activation function, to scale the output values to
	* a smooth & standardized activation range. This determines to what degree each
	* neuron is considered to be "firing" or "activating". Applying the activation
	* function does not affect the structure/order of the elements in the matrix/vector;
	* only their values.
	*
	*        | S1 |     | fn_a( S1 ) |   | O1 |
	* fn_a ( |    | ) = |            | = |    |
	*        | S2 |     | fn_a( S2 ) |   | O2 |
	*
	* Back Propagation:
	* -----------------
	*
	* Training the network constitutes adjusting the weights in all layers in the
	* direction which reduces the error, and in proportion to their affect on the result.
	* I.e. weights that only contributed a little are only adjusted a little, etc. The
	* calculation matrix is very similar to the forward calculations, except in reverse;
	* starting at the end result of the network, and traveling backwards through the
	* nodes & weights, towards the beginning.
	*
	* First, we use the Target values from the training data to calculate the output error;
	* this also corresponds to the first layer (in reverse order, i.e. the last layer in
	* forward order):
	*
	* | T1 |   | O1 |   | E1 |
	* |    | - |    | = |    |
	* | T2 |   | O2 |   | E2 |
	*
	* Then, we use this error vector to feed backwards through the network, to distribute
	* the error amongst all the nodes, based upon their influence/responsibility for that
	* error (i.e. their weights).
	*
	*              [B1]
	*              /
	*             /
	* [E1] --> (N3) <--W1-- (N1)
	*                 \  /W2
	*                  \/
	*                  /\
	*                 /  \W3
	* [E2] --> (N4) <--W4-- (N2)
	*            \
	*             \
	*             [B2]
	*
	*
	* Error Calculation Matrix:
	* -------------------------
	*
	* Since, matrix multiplication is Fixed Direction Specific, and since we are now going
	* backwards through the network, we have to transpose the weight matrices before
	* multiplying them with the error vectors.
	*
	*
	* N1 Weights Row --> | W1   W2 |    | E1 |
	*                    |         | ** |    | =
	* N2 Weights Row --> | W3   W4 |    | E2 |
	*                      ^    ^
	*                      |    |
	*                     N3    N4
	*                 Source    Source
	*                 Column    Column
	*
	* ========
	*
	* @param LAYOUT An array of integers indicating the number of nodes at each layer of
	*               the network. The first layer is the number of input nodes, and the
	*               last layer is the number of output nodes. All layers in between, are
	*               the hidden layers. The order is from left to right, starting with the
	*               input nodes, then the hidden layers, then ending with the output nodes.
	*
	*               Ex: (2,3,3,1) -> a network with 2 input nodes, 1 output node, and two
	*               hidden layers, each consisting of 3 nodes.
	*/
class NN(LAYOUT: IndexedSeq[Int]) {
	val rand = new Random(123)
	val layers = generateLayers()
	val a = 1

	def generateLayers(): IndexedSeq[(Matrix, Vector)] = {
		//Turn the LAYOUT list, into a list of tuples of the gaps between the elements.
		//I.e. (1,2,3,4) => ( (1,2), (2,3), (3,4) )
		//Then pass that in, to use as the dimensions for the weight matrices in the layers.
		//
		//Start at the second element (index 1), and iterate until the last element of the list
		//to prevent ArrayIndexOutOfBounds.
		val gapList = for(i <- 1 until LAYOUT.length)
											yield (
												LAYOUT(i-1),
												LAYOUT(i)
											)

		/*
		 * Weight matrix dimensions are:
		 *
		 * Num Rows    = Num Output Nodes
		 * Num Columns = Num Input Nodes
		 *
		 * For more explanation, see the main class doc.
		 */
		return for((numInputNodes,numOutputNodes) <- gapList)
							yield (
								//Weights
								new Matrix(
									Array.fill(numInputNodes * numOutputNodes)
										{rand.nextDouble}
										.toIndexedSeq
										.grouped(numInputNodes)
										.toIndexedSeq
								),
								//Biases
								new Vector(
									Array.fill(numOutputNodes){rand.nextDouble},
									vertical = true
								)
							)
	}

	/**
		* Supervised training
		*
		* @param init_input
		* @param target
		*/
	def train(init_input: Vector, target: Vector): Vector = {
		val (predictions, intermediateState) = feedForward(init_input)

		val error = target - predictions

		(layers zip intermediateState).foldRight(error) {
			case((weights, biases), (sum, activatedSum), error) => {


				//Return the calculated error for the next-previous layer
			}
		}

		//TODO testing
		println("\nInput:\n---")
		init_input.println

		println("\nOutput:\n---")
		predictions.println

		println("\nTarget:\n---")
		target.println

		println("\nError:\n---")
		error.println



		return error.toVector
	}

	/*
	 * A matrix multiplied by a vector always results in a vector output.
	 * Since, that is the format of our calculation (Weights matrix * input vector),
	 * the result of this function will always be a vector.
	 */
	def feedForward(init_input: Vector): (Matrix, ListBuffer[(Vector,Vector)]) = {
		println("\ninit_input:\n---\n")
		init_input.print

		val result = layers.foldLeft((init_input:Matrix, ListBuffer[(Vector,Vector)]())) {
			case (
				(inputs:Matrix, intermediateStateList),
				(weights:Matrix, biases:Vector)
			) => {
				println("\nWeights:\n---\n")
				weights.print

				println("\nInputs:\n---\n")
				inputs.print

				val sum = (weights ** inputs) + biases
				val activatedSum = activate(sum).toVector

				//Log State
				intermediateStateList.append(
					(sum.toVector, activatedSum)
				)

				//Feed activated output matrix as input to the next layer
				(activatedSum, intermediateStateList)
			}
		}

		//Convert result back to vector for return
		return result
	}

	/**
		* The sigmoid function used is:
		*
		*       1
		* ------------
		*  1 - e^(-x)
		*
		* @param weightBiasSum
		*/
	def activate(weightBiasSum: Matrix): Matrix = {
		return weightBiasSum.map((x) => { 1 / (1 - Math.exp(x)) })
	}

	def print(): Unit = {
		layers.foreach {
			case(weights, biases) => {
				println("\n===\n\nWeights:\n---")
				weights.print

				println("\nBiases:\n---")
				biases.print
			}
		}
		println("\n===")
	}
}
