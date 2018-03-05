import javax.swing.UIManager

import nn.NN
import nn.matrix.Vector

import scala.util.Random

import scala.swing.{BorderPanel, Component, Graphics2D, MainFrame, SimpleSwingApplication}

object Graph2D_XOR extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

//	val rand = new Random(12345*321)
	val rand = Random

	//Init Network
//	val brain = new NN(IndexedSeq(2,3,4), LR = 0.01)

	val brain_XOR = new NN(IndexedSeq(2,2,1), LR = 0.1)
	val inputTarget_XOR = IndexedSeq[(IndexedSeq[Double],IndexedSeq[Double])](
		IndexedSeq[Double](0,0) -> IndexedSeq[Double](0),//Output: OR, XOR, AND, NAND (Not And)
		IndexedSeq[Double](0,1) -> IndexedSeq[Double](1),
		IndexedSeq[Double](1,0) -> IndexedSeq[Double](1),
		IndexedSeq[Double](1,1) -> IndexedSeq[Double](0)
	).map{
		case (inputs,targets) => {
			(new Vector(inputs, vertical = true), new Vector(targets, vertical = true))
		}
	}

//	val brain_All = new NN(IndexedSeq(2,3,4), LR = 0.1)
//	val inputTarget_All = IndexedSeq[(IndexedSeq[Double],IndexedSeq[Double])](
//		IndexedSeq[Double](0,0) -> IndexedSeq[Double](0,0,0,1),//Output: OR, XOR, AND, NAND (Not And)
//		IndexedSeq[Double](0,1) -> IndexedSeq[Double](1,1,0,1),
//		IndexedSeq[Double](1,0) -> IndexedSeq[Double](1,1,0,1),
//		IndexedSeq[Double](1,1) -> IndexedSeq[Double](1,0,1,0)
//	).map{
//		case (inputs,targets) => {
//			(new Vector(inputs, vertical = true), new Vector(targets, vertical = true))
//		}
//	}

	val brain = brain_XOR
	val inputTarget = inputTarget_XOR

	//Test Feed Forward
//	brain.print()
//	val (results, intermediateState) = brain.feedForward(inputTarget(0)._1)
//	println("\nResults:\n---\n")
//	results.print

	//Test Train
//	brain.print()
//	val error = brain.train(inputTarget(0)._1, inputTarget(0)._2)
//	println("\nError:\n---\n")
//	error.print


	{//TODO Network is actively learning in the wrong direction.
		//TODO To see, fix the random index to 1 of the nodes in the training data.
		//TODO Run the training for a few thousand times, only using that single
		//TODO data point. The network will seek the highest error, reguardless
		//TODO of which point you choose. If you invert the weights equation in
		//TODO the train function to: curWeights - deltaWeights (rather than adding)
		//TODO the network will successfully learn IF and only IF you train on a
		//TODO single point. If you train on random points, it will seek the highest
		//TODO average error, i.e. returning 0.5 for all inputs.
		val numTimes = 5000
		println(s"\nTraining $numTimes times\n======\n")
		val (testInput, testTarget) = inputTarget(1)
		val (preTrainPrediction, _) = brain.feedForward(testInput)
		val preTrainError = testTarget - preTrainPrediction

		println("Pre-Train\nError:\t"+preTrainError.asHorizontal)
		for (i <- 0 to numTimes) {
			val randomIndex = rand.nextInt(inputTarget.length)
			val (trainingInput, trainingTarget) = inputTarget(randomIndex)
			val (_, postTrainError) = brain.train(trainingInput, trainingTarget)

			//Only show stages
			if (i % (numTimes/10) == 0)
				printf(f"$i%4d:\t"+postTrainError.asHorizontal+"\n")
		}
	}

	//Print final accuracy
	println(s"\nFinal Accuracy:\n---\n")
	inputTarget.foreach {
		case(input, target) => {
			val (prediction, _) = brain.feedForward(input)
			println(s"input: ${input.asHorizontal}\t->\t\ttarget: ${target.asHorizontal}\t=>\t\tprediction: ${prediction}")
		}
	}

	val canvas = new Component {



		override def paintComponent(g: Graphics2D): Unit = {
			//Yield to Parent
			super.paintComponent(g)

		}
	}

	var panel = new BorderPanel {
		layout(canvas) = BorderPanel.Position.Center
	}

	def top = new MainFrame {
		title = "XOR Test"
		contents = panel
	}
}
