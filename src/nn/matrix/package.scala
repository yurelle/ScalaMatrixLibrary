package nn

package object matrix {
	//Pimp My Library
	//See: http://docs.scala-lang.org/overviews/core/value-classes.html
	//See: https://coderwall.com/p/k_1jzw/scala-s-pimp-my-library-pattern-example
	//
	//Generic Numeric
	//See: https://github.com/ghik/opinionated-scala/wiki/Generics-and-type-members
	//See: https://stackoverflow.com/a/2336030/7206367
	//
	// Can't get to work
	//	trait HasNum[T] {
	//		def num:Numeric[T]
	//	}
	//
	//	trait MatrixAware[T] extends HasNum[T] {
	//		def +[B <:Numeric[T]](that:Matrix): Matrix = that.map(_ + num)
	//		def -(that:Matrix): Matrix = that.map(_ - num)
	//		def *(that:Matrix): Matrix = that.map(_ * num)
	//		def /(that:Matrix): Matrix = that.map(_ / num)
	//	}

	/*
	 * Vector
	 */
	implicit class VectorAwareDouble(val num: Double) extends AnyVal {
		def +(that:Vector): Vector = that.map(_ + num)
		def -(that:Vector): Vector = that.map(_ - num)
		def *(that:Vector): Vector = that.map(_ * num)
		def /(that:Vector): Vector = that.map(_ / num)
	}
	implicit class VectorAwareFloat(val num: Float) extends AnyVal {
		def +(that:Vector): Vector = that.map(_ + num)
		def -(that:Vector): Vector = that.map(_ - num)
		def *(that:Vector): Vector = that.map(_ * num)
		def /(that:Vector): Vector = that.map(_ / num)
	}
	implicit class VectorAwareLong(val num: Long) extends AnyVal {
		def +(that:Vector): Vector = that.map(_ + num)
		def -(that:Vector): Vector = that.map(_ - num)
		def *(that:Vector): Vector = that.map(_ * num)
		def /(that:Vector): Vector = that.map(_ / num)
	}
	implicit class VectorAwareInt(val num: Int) extends AnyVal {
		def +(that:Vector): Vector = that.map(_ + num)
		def -(that:Vector): Vector = that.map(_ - num)
		def *(that:Vector): Vector = that.map(_ * num)
		def /(that:Vector): Vector = that.map(_ / num)
	}

	/*
	 * Matrix
	 */
	implicit class MatrixAwareDouble(val num: Double) extends AnyVal {
		def +(that:Matrix): Matrix = that.map(_ + num)
		def -(that:Matrix): Matrix = that.map(_ - num)
		def *(that:Matrix): Matrix = that.map(_ * num)
		def /(that:Matrix): Matrix = that.map(_ / num)
	}
	implicit class MatrixAwareFloat(val num: Float) extends AnyVal {
		def +(that:Matrix): Matrix = that.map(_ + num)
		def -(that:Matrix): Matrix = that.map(_ - num)
		def *(that:Matrix): Matrix = that.map(_ * num)
		def /(that:Matrix): Matrix = that.map(_ / num)
	}
	implicit class MatrixAwareLong(val num: Long) extends AnyVal {
		def +(that:Matrix): Matrix = that.map(_ + num)
		def -(that:Matrix): Matrix = that.map(_ - num)
		def *(that:Matrix): Matrix = that.map(_ * num)
		def /(that:Matrix): Matrix = that.map(_ / num)
	}
	implicit class MatrixAwareInt(val num: Int) extends AnyVal {
		def +(that:Matrix): Matrix = that.map(_ + num)
		def -(that:Matrix): Matrix = that.map(_ - num)
		def *(that:Matrix): Matrix = that.map(_ * num)
		def /(that:Matrix): Matrix = that.map(_ / num)
	}

}
