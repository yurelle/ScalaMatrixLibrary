package nn.matrix

//See: https://www.scala-lang.org/api/2.12.x/scala/collection/immutable/IndexedSeq.html
//See: https://medium.com/@hussachai/scalas-immutable-collections-can-be-slow-as-a-snail-da6fc24bc688
class Matrix_1D_Impl(val ROWS:Int, val COLS:Int, val data: IndexedSeq[Double]) {
	val numElements = ROWS*COLS


	def this(ROWS:Int, COLS:Int) {
		this(ROWS, COLS, Array.fill(ROWS*COLS){0.0}.toIndexedSeq)
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

//	def getCoordinatesOfIndex(index:Int): (Int, Int) = {
//		val curRow = getRowForIndex(index)
//		val curCol = getColForIndex(index)
//
//		return (curRow, curCol)
//	}

	//Retrieve Value
	def apply(rowIndex:Int, colIndex:Int): Number = {
		val index = getIndexForRowCol(rowIndex, colIndex)
		return this.data(index)
	}

	//Set Value
//	def update(coord: Coordinate, e: Number): Unit = {
//		val index = getIndexForRowCol(coord)
//		this.data.update(index, e)
//	}

	def interleave(that: Matrix_1D_Impl, operation: (Double, Double) => Double): Matrix_1D_Impl = {
		//1D
		val resultData = (this.data zip that.data).map {
			case (e1, e2) => operation(e1, e2)
		}.toIndexedSeq

		return new Matrix_1D_Impl(ROWS, COLS, resultData)
	}

	//Element-wise Addition
	def +(that: Matrix_1D_Impl): Matrix_1D_Impl = {
		return interleave(that, (x,y) => x+y )
	}

	//Element-wise Subtraction
	def -(that: Matrix_1D_Impl): Matrix_1D_Impl = {
		return interleave(that, (x,y) => x-y )
	}

	//Element-wise Multiplication
	def x(that: Matrix_1D_Impl): Matrix_1D_Impl = {
		return interleave(that, (x,y) => x*y )
	}

	//Element-wise Division
	def /(that: Matrix_1D_Impl): Matrix_1D_Impl = {
		return interleave(that, (x,y) => x/y )
	}

	//Dot Product //TODO - implement
	def *(that: Matrix_1D_Impl): Matrix_1D_Impl = {
		val thatT = that.transpose

		for(rowIndex <- (0 until ROWS)) {

		}

		//Output dimensions are a merger of the two input matrices
		val resultData = new Array[Double](this.ROWS * that.COLS)

		return new Matrix_1D_Impl(COLS, ROWS, resultData)
	}

	//Transpose
	def transpose: Matrix_1D_Impl = {
		val resultData = new Array[Double](numElements)

		this.data.zipWithIndex.foreach{
			case (e,i) => {
				val rowIndex = getRowForIndex(i)
				val colIndex = getColForIndex(i)

				//TODO extract the index calculation to a stateful utility (super class?) so you can keep the Matrix
				//TODO object immutable, without having to create 2 of them here.
				val toIndex = new Matrix_1D_Impl(COLS, ROWS).getIndexForRowCol(colIndex, rowIndex)//rotated
				resultData(toIndex) = e
			}
		}

		return new Matrix_1D_Impl(COLS, ROWS, resultData)//Rotated
	}

	//1D
	def print = {
		println()
		val lastRowIndex = COLS-1;

		this.data.zipWithIndex.foreach{
			case(e,i) => {
				if ((i%COLS) == 0) {
					printf("| ")
				} else {
					printf("\t")
				}

				//See: https://docs.scala-lang.org/overviews/core/string-interpolation.html
				printf(f"$e%10.2f")

				if ((i%COLS) == lastRowIndex) {
					printf(" |\n")
				}
			}
		}
	}

}