import javax.swing.UIManager

import GraphXOR.canvas
import nn.NN

import scala.swing.{BorderPanel, MainFrame, SimpleSwingApplication}

object Graph_Parabolic extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

	//Init Network
	val brain = new NN(IndexedSeq())



	var panel = new BorderPanel {
		layout(canvas) = BorderPanel.Position.Center
	}

	def top = new MainFrame {
		title = "XOR Test"
		contents = panel
	}
}
