package nn.matrix

//See: https://www.scala-lang.org/api/2.12.x/scala/collection/immutable/IndexedSeq.html
//See: https://medium.com/@hussachai/scalas-immutable-collections-can-be-slow-as-a-snail-da6fc24bc688
//
//Arrays don't fully implement collections API, and break the lazy-view chain. Convert to a full-collection type.
//See: https://stackoverflow.com/questions/40517391/scala-view-force-is-not-a-member-of-seq
//
//Possible Numeric Generics
//See: https://typelevel.org/blog/2013/07/07/generic-numeric-programming.html
class Matrix(data: IndexedSeq[IndexedSeq[Double]])
	extends DataSet(data) {

	def this(ROWS:Int, COLS:Int) {
		this(Array.fill(ROWS*COLS){0.0}.toIndexedSeq.grouped(COLS).toIndexedSeq)
	}

	def map(operation: (Double) => Double): Matrix = {
		val resultData = this.data.map(_.map(e => operation(e)).toIndexedSeq)
		return new Matrix(resultData)
	}

	//Forward element access notation to inner data structure. This allows treating the matrix like a native
	//data structure (i.e. array, etc.). Ex: val myElement = myMatrix(3)(2)
	def apply(rowIndex:Int): IndexedSeq[Double] = {
		return this.data(rowIndex)
	}

	//Transpose
	def transpose: Matrix = new Matrix(this.data.transpose)

	//Element-wise Dual Matrix Functions
	def +(that: Matrix): Matrix = new Matrix(interleave(that, _+_))
	def -(that: Matrix): Matrix = new Matrix(interleave(that, _-_))
	def *(that: Matrix): Matrix = new Matrix(interleave(that, _*_))
	def /(that: Matrix): Matrix = new Matrix(interleave(that, _/_))

	//Element-wise Scalar Functions
	def +(num: Double): Matrix = this.map(_ + num)
	def +(num: Float): Matrix = this.map(_ + num)
	def +(num: Long): Matrix = this.map(_ + num)
	def +(num: Int): Matrix = this.map(_ + num)

	def -(num: Double): Matrix = this.map(_ - num)
	def -(num: Float): Matrix = this.map(_ - num)
	def -(num: Long): Matrix = this.map(_ - num)
	def -(num: Int): Matrix = this.map(_ - num)
	def unary_-(): Matrix = this.map(0 - _)

	def *(num: Double): Matrix = this.map(_ * num)
	def *(num: Float): Matrix = this.map(_ * num)
	def *(num: Long): Matrix = this.map(_ * num)
	def *(num: Int): Matrix = this.map(_ * num)

	def /(num: Double): Matrix = this.map(_ / num)
	def /(num: Float): Matrix = this.map(_ / num)
	def /(num: Long): Matrix = this.map(_ / num)
	def /(num: Int): Matrix = this.map(_ / num)

	//Dot Product
	//
	//A Matrix times a vector always results in a vector
	//TODO unless the matrix is actually a vector masquerading a matrix.
	//TODO I.e. a Matrix object with only 1 column, or only 1 row.
	//TODO This should never happen in our usage of the code, and this is
	//TODO just a throwaway experiment, so probably don't need to fix.
	def **(that: Vector): Vector = {
		return new Matrix(this._doDotProduct(that)).toVector
	}
	def **(that: Matrix): Matrix = {
		return new Matrix(super._doDotProduct(that))
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
}