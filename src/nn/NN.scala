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
	* PL_E: Previous Layer Error
	* LR: Learning Rate
	* ▲: Delta; i.e. "Change" (Type Alt+30, Windows with US SYSTEM LOCAL)
	* **: Matrix Multiplication
	* afn(): Activation Function
	* afn'(): Derivative of Activation Function
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
	*                         W      **    I   +   B    =   S
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
	*       | S1 |     | afn( S1 ) |   | O1 |
	* afn ( |    | ) = |           | = |    |
	*       | S2 |     | afn( S2 ) |   | O2 |
	*
	* The activation function used is the sigmoid function:
	*
	*       1
	* ------------
	*  1 - e^(-x)
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
	*   T    -   O    =   E
	*
	* | T1 |   | O1 |   | E1 | <-- N3 Error
	* |    | - |    | = |    |
	* | T2 |   | O2 |   | E2 | <-- N4 Error
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
	* 1. Calculate next layer's error using initial (unmodified) weights. This is not used in this
	* iteration of the calculation, it is simply passed on to the next iteration. However, we have
	* to do it first, since we need to use the initial unmodified weights, before we update them.
	*
	*                    (Weigh Matrix
	*                    Transposed)
	*
	*                        W_T     **   E    =   PL_E
	*
	* N1 Weights Row --> | W1   W2 |    | E1 |   | PL_E1 | <-- N1 Error (Previous Layer Error 1)
	*                    |         | ** |    | = |       |
	* N2 Weights Row --> | W3   W4 |    | E2 |   | PL_E2 | <-- N2 Error (Previous Layer Error 2)
	*                      ^    ^
	*                      |    |
	*                     N3    N4
	*                 Source    Source
	*                 Column    Column
	*
	* 2. Calculate the gradient descent ▲Weights. Since we're going backwards, we also need the
	* "reverse" of the activation function; i.e. it's derivative. The derivative of our sigmoid
	* function is:
	*
	* afn'(x) = afn(x) * (1 - afn(x))
	*
	* But, since we're keeping track of the internal state, we already have the value of afn(x),
	* stored as the Sum before activation. We can reuse that, to make the function:
	*
	* afn'(x) = S * (1 - S)
	*
	* So, our matrix equation is:
	*                                        (Input Vector
	*                                         Transposed)
	*
	*    LR    *    E    *    afn'(S)     **     I_T     =       ▲W
	*
	*             | E1 |         | S1 |                    | ▲W1   ▲W2 |
	*    LR    *  |    | * afn'( |    | ) ** | I1   I2 | = |            |
	* (Scalar)    | E2 |         | S2 |                    | ▲W3   ▲W4 |
	*
	*
	*
	*
	*
	*
	*
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
class NN(LAYOUT: IndexedSeq[Int], LR: Double) {
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

	/*
	 * A matrix multiplied by a vector always results in a vector output.
	 * Since, that is the format of our calculation (Weights matrix * input vector),
	 * the result of this function will always be a vector.
	 */
	def feedForward(init_input: Vector): (Vector, ListBuffer[(Vector,Vector,Vector)]) = {
//		println("\ninit_input:\n---\n")
//		init_input.print

		val result = layers.foldLeft((init_input:Vector, ListBuffer[(Vector,Vector,Vector)]())) {
			case (
				(inputs:Vector, intermediateStateList),
				(weights:Matrix, biases:Vector)
			) => {
//				println("\nWeights:\n---\n")
//				weights.print
//
//				println("\nInputs:\n---\n")
//				inputs.print
//
//				println("\n===")

				val sum = (weights ** inputs) + biases
				val activatedSum = activate(sum).toVector

				//Log State
				intermediateStateList.append(
					(inputs, sum.toVector, activatedSum)
				)

				//Feed activated output matrix as input to the next layer
				(activatedSum, intermediateStateList)
			}
		}

		//Convert result back to vector for return
		return result
	}

	/**
		* Supervised training
		*
		* @param init_input
		* @param target
		*/
	def train(init_input: Vector, target: Vector): Vector = {
		val (predictions, intermediateState) = feedForward(init_input)

		val init_error = target - predictions

		//Propagate Backwards
		(layers zip intermediateState).foldRight(((ListBuffer[Matrix](), ListBuffer[Vector]()), init_error)) {
			case(
				(
					(init_weights, init_biases),
					(inputs, sum, activatedSum)
				),
				(
					(newWeights, newBiases),
					error
				)
			) => {
//				println("\nWeights:\n---\n")
//				weights.print
//
//				println("\nBiases:\n---\n")
//				biases.print

//				println("\nSum:\n---\n")
//				sum.print

//				println("\nActivatedSum:\n---\n")
//				activatedSum.print

//				println("\nError:\n---\n")
//				error.print

//				println("\n===")

				val w_T = init_weights.transpose
				val i_T = inputs.transpose
				val nextError = w_T ** error
				val derivative = activatedSum * (1 - activatedSum)
				val gradient = LR * error * derivative
				val weightsDelta = gradient ** i_T
				val biasesDelta = gradient //Bias input is always 1, so we can ignore it

				newWeights.append(init_weights + weightsDelta)
				newBiases.prepend(init_biases + biasesDelta)

				//Return the calculated error for the next-previous layer
				((newWeights, newBiases), nextError)
			}
		}

		//TODO testing
		println("\nInput:\n---")
		init_input.println

		println("\nOutput:\n---")
		predictions.println

		println("\nTarget:\n---")
		target.println

		println("\nInitial Error:\n---")
		init_error.println



		return init_error.toVector
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
