import javax.swing.UIManager

import nn.NN

import scala.swing.{BorderPanel, Component, Graphics2D, MainFrame, SimpleSwingApplication}

object Graph2D_Parabolic extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

	//Init Network
	val brain = new NN(IndexedSeq(), 0.01)

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
