package nn.matrix

//See: https://www.scala-lang.org/api/2.12.x/scala/collection/immutable/IndexedSeq.html
//See: https://medium.com/@hussachai/scalas-immutable-collections-can-be-slow-as-a-snail-da6fc24bc688
//
//Arrays don't fully implement collections API, and break the lazy-view chain. Convert to a full-collection type.
//See: https://stackoverflow.com/questions/40517391/scala-view-force-is-not-a-member-of-seq
//
//Possible Numeric Generics
//See: https://typelevel.org/blog/2013/07/07/generic-numeric-programming.html
class Matrix(val data: IndexedSeq[IndexedSeq[Double]]) {
	val ROWS:Int = data.length
	val COLS:Int = if (ROWS > 0) data(0).length else 0

	def this(ROWS:Int, COLS:Int) {
		this(Array.fill(ROWS*COLS){0.0}.toIndexedSeq.grouped(COLS).toIndexedSeq)
	}

	def getRowForIndex(index:Int): Int = {
		return Math.floor(index / COLS).toInt
	}

	def getColForIndex(index:Int): Int = {
		return index % COLS;
	}

	def getIndexForRowCol(rowIndex:Int, colIndex:Int): Int = {
		return (rowIndex * COLS) + colIndex
	}

	//Forward element access notation to inner data structure. This allows treating the matrix like a native
	//data structure (i.e. array, etc.). Ex: val myElement = myMatrix(3)(2)
	def apply(rowIndex:Int): IndexedSeq[Double] = {
		return this.data(rowIndex)
	}

	//Set Value
//	def update(coord: Coordinate, e: Number): Unit = {
//		val index = getIndexForRowCol(coord)
//		this.data.update(index, e)
//	}

	def interleave(that: Matrix, operation: (Double, Double) => Double): Matrix = {
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

		return new Matrix(resultData)
	}

	def map(operation: (Double) => Double): Matrix = {
		val resultData = this.data.map(_.map(e => operation(e)).toIndexedSeq)
		return new Matrix(resultData)
	}

	//Element-wise Addition
	def +(that: Matrix): Matrix = {
		return interleave(that, (x,y) => x+y )
	}

	//Element-wise Subtraction
	def -(that: Matrix): Matrix = {
		return interleave(that, (x,y) => x-y )
	}

	//Element-wise Multiplication
	def *(that: Matrix): Matrix = {
		return interleave(that, (x,y) => x*y )
	}

	//Element-wise Division
	def /(that: Matrix): Matrix = {
		return interleave(that, (x,y) => x/y )
	}

	//Dot Product
	def **(that: Matrix): Matrix = {
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

		return new Matrix(resultData)
	}

	//Transpose
	def transpose: Matrix = {
		return new Matrix(this.data.transpose)//Rotated
	}

	def toVector: Vector = {
		val dataT = this.data.transpose

		//Only allow 1D or empty data
		require(dataT.length <= 1, s"dataT.length '${dataT.length}' is greater than 1")

		return new Vector(dataT(0))
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