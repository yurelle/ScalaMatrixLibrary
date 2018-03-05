package nn.matrix

//See: https://www.scala-lang.org/api/2.12.x/scala/collection/immutable/IndexedSeq.html
//See: https://medium.com/@hussachai/scalas-immutable-collections-can-be-slow-as-a-snail-da6fc24bc688
//
//Arrays don't fully implement collections API, and break the lazy-view chain. Convert to a full-collection type.
//See: https://stackoverflow.com/questions/40517391/scala-view-force-is-not-a-member-of-seq
//
//Possible Numeric Generics
//See: https://typelevel.org/blog/2013/07/07/generic-numeric-programming.html
class DataSet(val data: IndexedSeq[IndexedSeq[Double]]) {
	val ROWS:Int = data.length
	val COLS:Int = if (ROWS > 0) data(0).length else 0

	def this(ROWS:Int, COLS:Int) {
		this(Array.fill(ROWS*COLS){0.0}.toIndexedSeq.grouped(COLS).toIndexedSeq)
	}

	//Forward element access notation to inner data structure. This allows treating the matrix like a native
	//data structure (i.e. array, etc.). Ex: val myElement = myMatrix(3)(2)
//	def apply(rowIndex:Int): IndexedSeq[Double] = {
//		return this.data(rowIndex)
//	}

	def interleave(that: DataSet, operation: (Double, Double) => Double): IndexedSeq[IndexedSeq[Double]] = {
		require(this.ROWS == that.ROWS && this.COLS == that.COLS,
			s"Cannot perform element-wise operation; Matrices are different dimensions! " +
			s"A[rows:${this.ROWS}, cols:${this.COLS}] -> B[rows:${that.ROWS}, cols:${that.COLS}]")

		//2D - Super Fast - All On One Line
//		val resultData = (this.data zip that.data).map({case (a1,a2) => (a1 zip a2).map{case (e1, e2) => operation(e1, e2)}.toIndexedSeq})
		val resultData =
			(this.data zip that.data)
			.map({
				case (a1,a2) => {
					(a1 zip a2).map{
						case (e1, e2) => operation(e1, e2)
					}.toIndexedSeq
				}
			})

		//2D - Super Fast - Broken Up
		//See: http://blog.bruchez.name/2011/10/scala-partial-functions-without-phd.html
		//See: https://alvinalexander.com/scala/fp-book-diffs-val-def-scala-functions
//		val mapCols: PartialFunction[(Double,Double),Double] = {
//			case (e1, e2) => operation(e1, e2)
//		}
//		val mapRows: PartialFunction[(IndexedSeq[Double],IndexedSeq[Double]),IndexedSeq[Double]] = {
//			case (a1,a2) => (a1 zip a2).map(mapCols).toIndexedSeq
//		}
//		val resultData = (this.data zip that.data).map(mapRows)


		//2D - Super Slow
//		val resultData = (this.data.flatten zip that.data.flatten).map {
//			case (e1, e2) => operation(e1, e2)
//		}.grouped(COLS).toIndexedSeq

		//return new Matrix(resultData)
		return resultData
	}

	def map(operation: (Double) => Double): DataSet = {
		val resultData = this.data.map(_.map(e => operation(e)).toIndexedSeq)
		return new DataSet(resultData)
	}

	//Transpose
	def transpose: DataSet = new DataSet(this.data.transpose)

	//Element-wise Dual Matrix Functions
	def +(that: DataSet): DataSet = new DataSet(interleave(that, _+_))
	def -(that: DataSet): DataSet = new DataSet(interleave(that, _-_))
	def *(that: DataSet): DataSet = new DataSet(interleave(that, _*_))
	def /(that: DataSet): DataSet = new DataSet(interleave(that, _/_))

	//Element-wise Scalar Functions
	def +(num: Double): DataSet = this.map(_ + num)
	def +(num: Float): DataSet = this.map(_ + num)
	def +(num: Long): DataSet = this.map(_ + num)
	def +(num: Int): DataSet = this.map(_ + num)

	def -(num: Double): DataSet = this.map(_ - num)
	def -(num: Float): DataSet = this.map(_ - num)
	def -(num: Long): DataSet = this.map(_ - num)
	def -(num: Int): DataSet = this.map(_ - num)

	def *(num: Double): DataSet = this.map(_ * num)
	def *(num: Float): DataSet = this.map(_ * num)
	def *(num: Long): DataSet = this.map(_ * num)
	def *(num: Int): DataSet = this.map(_ * num)

	def /(num: Double): DataSet = this.map(_ / num)
	def /(num: Float): DataSet = this.map(_ / num)
	def /(num: Long): DataSet = this.map(_ / num)
	def /(num: Int): DataSet = this.map(_ / num)

	//Dot Product
	//
	//A Matrix times a vector always results in a vector
	def **(that: Vector): Vector = {
		return this.**(that:DataSet).toVector
	}
	def **(that: DataSet): DataSet = {
		require(this.COLS == that.ROWS, s"this.COLS '${this.COLS}' is not equal to that.ROWS '${that.ROWS}'")

		//Rotate second matrix for easier iteration
		val thatTData = that.data.transpose

		//Output dimensions are a merger of the two input matrices
		val resultRows = this.ROWS
		val resultCols = that.COLS

		//Forcing the range to an iterator maintains the lazy evaluation through the call to grouped,
		//creating a GroupedIterator, rather than evaluating the whole range.
		val resultDimensions = (1 to (resultRows * resultCols)).iterator.grouped(resultCols)

		val resultData = resultDimensions.zipWithIndex.map {
			case (row, rowIndex) => {
				row.indices.map(
					(colIndex) => {
						(this.data(rowIndex) zip thatTData(colIndex)).foldLeft(0.0) {
							case (runningTotal, (thisElement, thatElement)) => {
								runningTotal + (thisElement * thatElement)
							}
						}
					}
				)
			}
		}.toIndexedSeq

		return new DataSet(resultData)
	}

	def toVector: Vector = {
		val vertical = this.ROWS > 1
		val shortDimension = if (vertical) this.COLS else this.ROWS
		val horizontalData = if (vertical) this.data.transpose else this.data
		val array1D = horizontalData(0)

		//Only allow 1D or empty data
		require(shortDimension <= 1, s"Matrix is not a vector; short dimension > 1! " +
			s"[rows: ${this.ROWS}, cols: ${this.COLS}]")

		return new Vector(array1D, vertical)
	}

	/*
	 * Because the System.out.println is just a global function named "println",
	 * implementing our own function called "println" overrides the whole namespace
	 * inside this class for "println". All calling code will still work fine; they
	 * can call the global println and matrix.println distinctly, but inside this
	 * class, we can only use "printf". Otherwise, scala will think we mean to call
	 * this function.
	 */
	def println = {
		printf("\n")
		print
		printf("\n")
	}

	def print = {
		//See: https://docs.scala-lang.org/overviews/core/string-interpolation.html
		//See: https://stackoverflow.com/questions/9439535/is-foreach-by-definition-guaranteed-to-iterate-the-subject-collection-sequential
		//See: https://stackoverflow.com/questions/11319111/fold-and-foldleft-method-difference (linear execution vs fork-tree)
		val output = this.data.foldLeft("")((s:String, row:IndexedSeq[Double]) => {
			s + (row.foldLeft("| ")((s:String, d:Double) => s + f"$d%10.2f\t") + "|\n")
		})

		printf(output)//output already ends in a new line; let calling code decide to add another
	}
}