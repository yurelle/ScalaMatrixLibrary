import javax.swing.UIManager

import nn.NN
import nn.matrix.Vector

import scala.swing.{BorderPanel, Component, Graphics2D, MainFrame, SimpleSwingApplication}

object Graph2D_XOR extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

	//Init Network
	val brain = new NN(IndexedSeq(2,3,4), LR = 0.01)
	val inputTarget = IndexedSeq[(IndexedSeq[Double],IndexedSeq[Double])](
		IndexedSeq[Double](0,0) -> IndexedSeq[Double](0,0,0,1),//Output: OR, XOR, AND, NAND (Not And)
		IndexedSeq[Double](0,1) -> IndexedSeq[Double](1,1,0,1),
		IndexedSeq[Double](1,0) -> IndexedSeq[Double](1,1,0,1),
		IndexedSeq[Double](1,1) -> IndexedSeq[Double](1,0,1,0)
	).map{
		case (inputs,targets) => {
			(new Vector(inputs, true), new Vector(targets, true))
		}
	}

	//Test Feed Forward
//	brain.print()
//	val (results, intermediateState) = brain.feedForward(inputTarget(0)._1)
//	println("\nResults:\n---\n")
//	results.print

	//Test Train
//	brain.print()
	val error = brain.train(inputTarget(0)._1, inputTarget(0)._2)
//	println("\nError:\n---\n")
//	error.print

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
