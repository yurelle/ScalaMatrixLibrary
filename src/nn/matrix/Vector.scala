package nn.matrix

/**
	* Orientation defaults to all elements on 1 row. To have a column vector,
	* simply call myVector.transpose which will return an (N,1) matrix.
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
	* @param LENGTH
	* @param values
	*/
class Vector private(val LENGTH:Int, values: IndexedSeq[IndexedSeq[Double]])
	extends Matrix(values) {

	/**
		* Only allow users of this class to create 1 dimensional vectors, but
		* we need to create a 2D data structure to map to the parent matrix
		* implementation.
		*
		* @param values
		*/
	def this(values: IndexedSeq[Double]) {
		this(values.length, IndexedSeq(values))
	}

	def this(LENGTH:Int) {
		this(Array.fill[Double](LENGTH){0}.toIndexedSeq)
	}
}

