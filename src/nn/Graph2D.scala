package nn

import java.awt.{BasicStroke, BorderLayout, Color, Font, RenderingHints}
import java.awt.geom.{AffineTransform, Line2D}
import javax.swing.UIManager

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import swing._
import scala.swing._
import scala.swing.Swing._
import scala.swing.event.MouseClicked
import scala.util.Random

object Graph2D extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

	//Init Perceptron
	//2D coordinates = 2 inputs for perceptron (bias is internally managed)
	val perceptron = new Perceptron(numInputs = 2, learningRate = 0.001)

	//Init Data
	val coordSpace = new Rectangle(-500, -500, 1000, 1000)
	val rand = Random
	val clickTrain = false
	val preRenderPoints = false
	var numPoints:Int = 0
	val testData = new ArrayBuffer[(Double, Double)]()

	//Generate Data Points
	initTestData()

	def initTestData() = {
		val dataSetSize = 1024*6

		//Determine Prerender Size
		var numData:Int = -1
		if (preRenderPoints) {
			numData = dataSetSize
		} else {
			numData = 0
		}

		//Generate Data
		for(_ <- 1 to numData) {
			genNewDataPoint()
		}
	}

	def genNewDataPoint() = {
		//Increment Point Total
		numPoints += 1

		//Generate Point
		testData.append(
				( rand.nextDouble() * coordSpace.width  + coordSpace.x,
					rand.nextDouble() * coordSpace.height + coordSpace.y)
		)
	}

	val TF_m = (rand.nextDouble() * rand.nextInt()) % 1
	val TF_b = (rand.nextDouble() * rand.nextInt()) % 250 //Don't use full range. We need to leave
	//some headroom so there's enough points above the line to catch the predictions to train with.
	def targetFunc(x: Double): Double = {
		return (TF_m * x) + TF_b
	}

	def test(x: Double, y: Double): Double = {
		val targetY = targetFunc(x)
		val targetActivation = if (y > targetY) 1 else -1

		return targetActivation
	}

	def testError(x: Double, y: Double, pGuess: Double): Double = {
		return test(x,y) - pGuess
	}

	def runTraining(): Unit = {
		for (i <- 1 to 50) {
			//Generate New Points
			if (!preRenderPoints) {
				genNewDataPoint()
			}

			//Train
			testData.foreach {
				case(x,y) => {
					perceptron.train(IndexedSeq(x,y), test(x,y))
				}
			}
		}
	}

	val canvas = new Component {
		preferredSize = (1000,1000)
		minimumSize = preferredSize
		maximumSize = preferredSize

		//Init Timer
		var isTaskRunning:Boolean = false
		val timer = new java.util.Timer()
		var task:java.util.TimerTask = null

		def stopTask() = {
			isTaskRunning = false

			if (task != null) task.cancel()
		}

		//Inti Mouse Listener
		listenTo(mouse.clicks)
		reactions += {
			case e: MouseClicked => {
				if (clickTrain) {
					runTraining()
					repaint()
				} else {
					if (isTaskRunning) {
						//Halt Training
						task.cancel();
					} else {
						//Init new training run (Can't reuse. Will throw exception if rescheduled)
						task = new java.util.TimerTask {
							def run() = {
								runTraining()
								repaint()
							};
						}

						//Run first training batch
						//
						//If not prerendering points, the paint error-check will immediately
						//kill the timer because there are zero points, and thus zero error.
						runTraining()

						//Schedule training task
						timer.purge()
						timer.schedule(task, 333, 333)
					}

					//Display Results
					repaint()

					//Toggle timer flag
					isTaskRunning = !isTaskRunning
				}
			}
		}

		override def paintComponent(g: Graphics2D): Unit = {
			//Yield to Parent
			super.paintComponent(g)

			//Translate Coordinate Space
			val (bounds, halfWidth, halfHeight) = translateCoordinateSpace(g)

			//Init Render Quality Settings
			val rh = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
			g.setRenderingHints(rh)

			//Draw Axes
			g.setColor(new Color(150,150,150))
			g.setStroke(new BasicStroke(1))
			g.drawLine(-halfWidth, 0, halfWidth, 0)
			g.drawLine(0, -halfHeight, 0, halfHeight)

			//Draw Target Line
			g.setColor(Color.BLUE)
			g.setStroke(new BasicStroke(2))
			g.drawLine(-500, targetFunc(-500).toInt, 500, targetFunc(500).toInt)

			//Draw Perceptron's Hypothesis
			g.setColor(Color.BLACK)
			g.setStroke(new BasicStroke(2))
			g.drawLine(-500, pf(-500), 500, pf(500))

			//Draw Data Points
			//drawDataPoint(g, (100,100), true)
			val sqError = testData.map {
				case (x, y) => {
					val pGuess = perceptron.feedForward(IndexedSeq(x,y))
					val error = testError(x,y,pGuess)

					(x.toInt, y.toInt, error)
				}
			}.map {
				case(x,y, error) => {
					drawDataPoint(g, (x,y), error)

					error
				}
			}.foldLeft[Double](0)(
				(sum, e) => {
					val scaledE = e/2 //Max error is 2, not 1
					sum + (scaledE * scaledE)
				}
			)
			val errorRate = sqError / testData.length
			//.exists(_ != 0)

			if (errorRate == 0) {
				stopTask()
			}

	//--Print Directions
			//Revert Y Axis Orientation (Otherwise, text prints upside down)
			g.scale(1.0, -1.0)

			//Init Font
			val text = "Left Click to start & stop training"
			val metrics = g.getFontMetrics()
			val fontSize = 20
			g.setFont(new Font("Tahoma", Font.PLAIN, fontSize))

			//Draw Text Background
			val textPadding = 10
			val lineHeight = metrics.getHeight() + textPadding
			val txtR = new Rectangle(0, 400, 330, lineHeight*2 + (textPadding*1.5).toInt)
			g.setStroke(new BasicStroke(20))
			g.setColor(Color.BLACK)
			g.drawRect(txtR.x, txtR.y, txtR.width, txtR.height)
			g.setColor(Color.CYAN)
			g.fillRect(txtR.x, txtR.y, txtR.width, txtR.height)

			//Draw Text
			g.setColor(Color.BLACK)
			var textPosition = 400 + lineHeight
			g.drawString(text, textPadding, textPosition)

			textPosition += lineHeight
			g.drawString(f"[$numPoints points] Error Rate: $errorRate%.6f", textPadding, textPosition)


			//Train Next Iteration (if we train before painting, we'll never paint the first frame)
			//runTraining() --- Being called in mouse activated timer
		}
	}

	/*
	 * The line function is NOT "y = mx + b", because that is not how the
	 * perceptron calculates it's weights. The perceptron makes its decision
	 * based upon a sum of it's weights & inputs, therefore the line underlying
	 * this decision, must also be described in this form (i.e. as a sum of all
	 * it's weights).
	 *
	 * Secondly, the perceptron makes an "Above"/"Below" decision for all given
	 * points, thus dividing the universe into 2 sets of points: those "Above",
	 * and those "Below". The perceptron's output is of the form -1 -> +1,
	 * where a negative value indicates "Below", and a positive value indicates
	 * "Above". Thus, a value ON the line, would have a value of ZERO.
	 *
	 * Therefore, the equation for this perceptron's line is a sum of the weights
	 * & inputs (using the same algorithm format used when activating the
	 * perceptron), AND this sum is set equal to ZERO, since we are trying to
	 * solve for the discerning line itself, rather than a data point "Above" or
	 * "Below" it.
	 *
	 * (Wx * X) + (Wy * Y) + (Wb * B) = 0
	 *
	 * In this function specifically, we want to be able to provide an X value
	 * and calculate its corresponding Y value. So, we algebraically solve for Y.
	 *
	 * (Wx * X) + (Wy * Y) + (Wb * B) = 0
	 * [ Flip sides to put Zero on left]
	 *
	 * 0 = (Wx * X) + (Wy * Y) + (Wb * B)
	 * [ Subtract Y Weight Compound Term: (Wy * Y) ]
	 *
	 * - Wy * Y = (Wx * X) + (Wb * B)
	 * [ Divide by Negative Y Weight: (-Wy) ]
	 *
	 * Y = ( (Wx * X) + (Wb * B) ) / (-Wy)
	 *
	 */
	def pf(x: Double): Int = {
		val xWeight = perceptron.weights(0)
		val yWeight = perceptron.weights(1)
		val biasWeight = perceptron.biasWeight

		//Bias value is always 1, so the multiplication by it is unnecessary; just biasWeight is fine.
		val y = ( (xWeight * x) + biasWeight ) / (-yWeight)

		return y.toInt
	}

	def translateCoordinateSpace(g: Graphics2D): (Rectangle, Int, Int) = {
		//Calc Center
		val bounds = g.getClipBounds
		val halfWidth = bounds.width/2
		val halfHeight = bounds.height/2

		//Translate Origin
		val origin = (halfWidth, halfHeight)
		(g.translate _).tupled(origin)

		//Flip Y Axis So That Positive Is Up
		//See: https://docstore.mik.ua/orelly/java-ent/jfc/ch04_03.htm
		g.scale(1.0, -1.0)

		//Return bounds vars
		return (bounds, halfWidth.toInt, halfHeight.toInt)
	}

	val dotSize = 5;
	val borderThickness = 2
	val borderDotSize = dotSize + (borderThickness*2)
	def drawDataPoint(g: Graphics2D, point: (Int, Int), error: Double): Unit = {
		//Draw Border
		g.setPaint(Color.BLACK)
		g.fillOval(
			point.x - borderDotSize/2,
			point.y - borderDotSize/2,
			borderDotSize,
			borderDotSize
		)

		//Fill Value
		val magnitude = translateRange(error, 0, 2, 0, 255)
		val brightness = Math.abs(magnitude).toInt
		g.setPaint(new Color(brightness, 255-brightness, 0))
//		g.setPaint(if (error > 0) new Color(0,255-brightness,0) else new Color(255-Math.abs(brightness),0,0))
		g.fillOval(
			point.x - dotSize/2,
			point.y - dotSize/2,
			dotSize,
			dotSize
		)
	}

	def translateRange(value: Double, srcFrom: Double, srcTo: Double, destFrom: Double, destTo: Double): Double = {
		val srcRange = srcTo - srcFrom
		val destRange = destTo - destFrom
		val srcOffset = value - srcFrom
		val srcPossition = srcOffset / srcRange
		val destOffset = srcPossition * destRange
		val destVal = destOffset + destFrom

		return destVal
	}

	var panel = new BorderPanel {
		layout(canvas) = BorderPanel.Position.Center
	}

	def top = new MainFrame {
		title = "Simple Line Test"
		contents = panel
	}
}
