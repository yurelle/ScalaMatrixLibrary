import java.awt.{Color, Dimension, Event}
import javax.swing.UIManager

import nn.NN
import nn.matrix.Vector

import scala.collection.mutable.ListBuffer
import scala.swing._
import scala.swing.event._
import scala.util.Random
import scala.swing.{BorderPanel, Component, Graphics2D, MainFrame, SimpleSwingApplication}

object Graph2D_XOR extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

	val rand = Random

	//Init Network: Full Binary
	val brain = new NN(IndexedSeq(2,2,4), LR = 0.1)
	val inputTarget = IndexedSeq[(IndexedSeq[Double],IndexedSeq[Double])](
		IndexedSeq[Double](0,0) -> IndexedSeq[Double](0,0,0,1),//Output: OR, XOR, AND, NAND (Not And)
		IndexedSeq[Double](0,1) -> IndexedSeq[Double](1,1,0,1),
		IndexedSeq[Double](1,0) -> IndexedSeq[Double](1,1,0,1),
		IndexedSeq[Double](1,1) -> IndexedSeq[Double](1,0,1,0)
	).map{
		case (inputs,targets) => {
			(new Vector(inputs, vertical = true), new Vector(targets, vertical = true))
		}
	}

	var INVERT_COORDS = false;
	val canvas = new Component {
		val dim = new Dimension(500,500)
		minimumSize = dim
		maximumSize = dim
		preferredSize = dim

		//Inti Mouse Listener
		listenTo(keys)
		listenTo(mouse.clicks)
		reactions += {
			case e: MouseClicked => {
				repaint()
			}
			case KeyPressed(_, Key.Space, _, _) => {
				INVERT_COORDS = !INVERT_COORDS
				repaint()
			}
		}
		focusable = true
		requestFocus

		class BinaryCmpResults(val ROW_INDEX: Int, val COL_INDEX: Int) {
			var OR:Double = _
			var XOR:Double = _
			var AND:Double = _
			var NAND:Double = _
		}

		override def paintComponent(g: Graphics2D): Unit = {
			//Yield to Parent
			super.paintComponent(g)

			//Run 1 training batch
			var postTrainError = null:Vector
			for(i <- 1 to 500) {
				val randomIndex = rand.nextInt(inputTarget.length)
				val (trainingInput, trainingTarget) = inputTarget(randomIndex)
				postTrainError = brain.train(trainingInput, trainingTarget)._2
			}

			//Calculate available canvas size
			val resolution = 10
			val halfWidth = dim.width / 2
			val halfHeight = dim.height / 2
			val rows = halfHeight / resolution
			val cols = halfWidth / resolution
			val rowValueScalar = rows - 1 //-1 for including both ends: Zero & One
			val colValueScalar = cols - 1 //-1 for including both ends: Zero & One

			//Row Index Matrix
			val rowIndexVector = new Vector(0.0 until rows by 1.0, vertical = true)
			val rowIdentityVector = new Vector(Array.fill[Double](cols){1}, vertical = false)
			val rowIndexMatrix = rowIndexVector ** rowIdentityVector

			//Column Index Matrix
			val colIdentityVector = new Vector(Array.fill[Double](cols){1}, vertical = true)
			val colIndexVector = new Vector(0.0 until cols by 1.0, vertical = false)
			val colIndexMatrix = colIdentityVector ** colIndexVector

			//Input Grid
			val inputGrid = rowIndexMatrix zip colIndexMatrix

			//Run Neural Network
			val resultData =
				inputGrid.map(gridRow => {
						gridRow.map{
							case (rowIndex, colIndex) => {
								val output = new BinaryCmpResults(ROW_INDEX = rowIndex.toInt,
																									COL_INDEX = colIndex.toInt)

								//Calculate Input value from index
								val rowInput = rowIndex / rowValueScalar
								val colInput = colIndex / colValueScalar

								//Generate Network Predictions
								val results = brain.feedForward(rowInput, colInput)._1
								output.OR   = results(0)
								output.XOR  = results(1)
								output.AND  = results(2)
								output.NAND = results(3)

								//Return Result
								output
							}
						}.toIndexedSeq
				})

			resultData.foreach(gridRow => {
				gridRow.foreach(outputs => {
					//Calculate coordinates
					val (px, py) = if (INVERT_COORDS) {
						(
							((cols - outputs.COL_INDEX)-1) * resolution,
							((rows - outputs.ROW_INDEX)-1) * resolution
						)
					} else {
						(
							outputs.COL_INDEX * resolution,
							outputs.ROW_INDEX * resolution
						)
					}

					//Paint Pixels
					g.setColor(getColor(outputs.OR))
					g.fillRect(px, py, resolution, resolution)

					g.setColor(getColor(outputs.XOR))
					g.fillRect(px + halfWidth, py, resolution, resolution)

					g.setColor(getColor(outputs.AND))
					g.fillRect(px, py + halfHeight, resolution, resolution)

					g.setColor(getColor(outputs.NAND))
					g.fillRect(px + halfWidth, py + halfHeight, resolution, resolution)
				})
			})

			def getColor(y:Double): Color = {
				val brightness = (y * 255).toInt
				return new Color(brightness, brightness, brightness)
			}
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
