package nn.matrix

/**
	* Orientation defaults to all elements on 1 row. To have a column vector,
	* simply call myVector.transpose, or use the constructor with the boolean
	* "vertical" flag.
	*
	* Note: We use this class rather than the built-in Scala Vector class,
	* so that we can use the bi-directional operator overloading defined in
	* the Matrix class without having to duplicate those definitions in the
	* Scala Vector class via Pimp-My-Library extension, which I also haven't
	* researched how to do yet. I'm sure there's probably a more elegant way
	* to do this, but I taught myself scala, functional programming, AND
	* neural networks in 3 days to write all this. I'm now continuing on to
	* the next step, but I'm still new to all this, so cut me some slack. I
	* think I did a pretty good job considering.
	*
	* @param values
	*/
class Vector private(values: IndexedSeq[IndexedSeq[Double]])
	extends DataSet(values) {

	/**
		* Only allow users of this class to create 1 dimensional vectors, but
		* we need to create a 2D data structure to map to the parent matrix
		* implementation.
		*/
	def this(values: IndexedSeq[Double], vertical: Boolean) {
		this(
			if (vertical)
				IndexedSeq(values).transpose
			else
				IndexedSeq(values)
		)
	}


	def this(length:Int, vertical: Boolean) {
		this(Array.fill[Double](length){0}.toIndexedSeq, vertical)
	}

	def this(LENGTH:Int) {
		this(LENGTH, false)
	}


	def map(operation: (Double) => Double): Vector = {
		val resultData = this.data.map(_.map(e => operation(e)).toIndexedSeq)
		return new Vector(resultData)
	}

	def transpose: Vector = new Vector(this.data.transpose)

	//Element-wise Dual Vector Functions
	def +(that: Vector): Vector = new Vector(interleave(that, _+_))
	def -(that: Vector): Vector = new Vector(interleave(that, _-_))
	def *(that: Vector): Vector = new Vector(interleave(that, _*_))
	def /(that: Vector): Vector = new Vector(interleave(that, _/_))

	//Element-wise Scalar Functions
	def +(num: Double): Vector = this.map(_ + num)
	def +(num: Float): Vector = this.map(_ + num)
	def +(num: Long): Vector = this.map(_ + num)
	def +(num: Int): Vector = this.map(_ + num)

	def -(num: Double): Vector = this.map(_ - num)
	def -(num: Float): Vector = this.map(_ - num)
	def -(num: Long): Vector = this.map(_ - num)
	def -(num: Int): Vector = this.map(_ - num)

	def *(num: Double): Vector = this.map(_ * num)
	def *(num: Float): Vector = this.map(_ * num)
	def *(num: Long): Vector = this.map(_ * num)
	def *(num: Int): Vector = this.map(_ * num)

	def /(num: Double): Vector = this.map(_ / num)
	def /(num: Float): Vector = this.map(_ / num)
	def /(num: Long): Vector = this.map(_ / num)
	def /(num: Int): Vector = this.map(_ / num)

	//Dot Product
	//
	//A Matrix times a vector always results in a vector
	def **(that:Matrix): Vector = {
		return new Vector(super._doDotProduct(that:Matrix))
	}

	/**
		* Depending upon the orientations of the input vectors, the output
		* of a Dot Product (i.e. matrix multiplication) between 2 vectors
		* will either be a single value, or a matrix. Since we have to set
		* a fixed return type, and can't overload based solly on return
		* type, we choose the greater of the two, and return a matrix.
		*
		*              | D |
		* [A, B, C] ** | E | = (AD + BC + CF) [Single Value]
		*              | F |
		*
		* | A |                  | AD   AE   AF |
		* | B |  **  [D, E, F] = | BD   BE   BF |
		* | C |                  | CD   CE   CF |
		*
		*
		* @param that
		* @return
		*/
	def **(that:Vector): Matrix = {
		return new Matrix(super._doDotProduct(that))
	}//TODO perhaps need to split this inheritance tree to a base class that both matrix & vector inheret from directly
	//TODO or finally refactor to pimp the standard Vector class

	def asHorizontal: Vector = {
		val isVertical = this.ROWS > 1

		if (isVertical) {
			return this.transpose
		} else {
			return this //Since Vector's are immutable, there's no risk reusing the same instance
		}
	}

	def asVertical: Vector = {
		val isVertical = this.ROWS > 1

		if (isVertical) {
			return this //Since Vector's are immutable, there's no risk reusing the same instance
		} else {
			return this.transpose
		}
	}
}

