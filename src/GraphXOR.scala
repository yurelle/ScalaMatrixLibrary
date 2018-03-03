import javax.swing.UIManager

import nn.Graph2D.canvas
import nn.NN
import nn.matrix.Vector

import scala.swing.{BorderPanel, Component, Graphics2D, MainFrame, SimpleSwingApplication}

object GraphXOR extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

	//Init Network
	val brain = new NN(IndexedSeq(2,2,1))
	val inputTarget = IndexedSeq[((Int,Int),Int)](
		(0,0) -> 0,
		(0,1) -> 1,
		(1,0) -> 1,
		(1,1) -> 0
	).map{
		case ((x1,x2),target) => {
			(new Vector(IndexedSeq[Double](x1.toDouble,x2.toDouble)),
			target)
		}
	}

	//Test
	brain.print()
	val results = brain.feedForward(inputTarget(0)._1)
	println("\nResults:\n---\n")
	results.print

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
