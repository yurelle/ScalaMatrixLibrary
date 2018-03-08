package nn.matrix

import nn.matrix.Vector

object MatrixTest extends App {
	/*
	 * Dual Matrix Tests
	 */
	{//Dual Matrix - Add Test
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = new Matrix((7.0 to 12.0 by 1.0).grouped(3).toIndexedSeq)
		val m3 = m1 + m2
		assert(listsEqual(
			m3.data,
			IndexedSeq[Double](8, 10, 12, 14, 16, 18).grouped(3).toIndexedSeq
		))
	}

	{//Dual Matrix - Subtract Test
	val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = new Matrix((12.0 to 7.0 by -1.0).grouped(3).toIndexedSeq)//if ascending result is all 6's
		val m3 = m1 - m2
		assert(listsEqual(
			m3.data,
			IndexedSeq[Double](-11, -9, -7, -5, -3, -1).grouped(3).toIndexedSeq
		))
	}

	{//Dual Matrix - Multiply Test
	val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = new Matrix((7.0 to 12.0 by 1.0).grouped(3).toIndexedSeq)
		val m3 = m1 * m2
		assert(listsEqual(
			m3.data,
			IndexedSeq[Double](7, 16, 27, 40, 55, 72).grouped(3).toIndexedSeq
		))
	}

	{//Dual Matrix - Divide Test
	val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = new Matrix((7.0 to 12.0 by 1.0).grouped(3).toIndexedSeq)
		val m3 = m1 / m2
		assert(listsEqual(
			m3.data,
			IndexedSeq[Double](1/7.0, 2/8.0, 3/9.0, 4/10.0, 5/11.0, 6/12.0).grouped(3).toIndexedSeq
		))
	}

	{//Dual Matrix - Dot Product Test 1
	val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = new Matrix((7.0 to 12.0 by 1.0).grouped(2).toIndexedSeq)
		val m3 = m1 ** m2
		assert(listsEqual(
			m3.data,
			IndexedSeq[Double](58, 64, 139, 154).grouped(2).toIndexedSeq
		))
	}

	{//Dual Matrix - Dot Product Test 2
	val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(2).toIndexedSeq)
		val m2 = new Matrix((7.0 to 12.0 by 1.0).grouped(3).toIndexedSeq)
		val m3 = m1 ** m2
		assert(listsEqual(
			m3.data,
			IndexedSeq[Double](27, 30, 33, 61, 68, 75, 95, 106, 117).grouped(3).toIndexedSeq
		))
	}

	/*
	 * End Dual Matrix Tests
	 */

	/*
	 * Matrix Vector Tests
	 */
	{//Matrix Vector - Dot Product Test 1
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val v1 = new Vector((7.0 to 9.0 by 1.0), vertical = true)
		val v2 = m1 ** v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](50, 122).grouped(1).toIndexedSeq
		))
	}
	{//Matrix Vector - Dot Product Test 1
	  val v1 = new Vector((7.0 to 9.0 by 1.0), vertical = false)
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(2).toIndexedSeq)
		val v2 = v1 ** m1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](76, 100).grouped(1).toIndexedSeq
		))
	}
	/*
	 * End Matrix Vector Tests
	 */

	/*
	 * Dual Vector Tests
	 */
	{//Dual Vector - Dot Product Test 1
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = new Vector((7.0 to 12.0 by 1.0), vertical = true)
		val m1 = v1 ** v2

		assert(m1.isInstanceOf[Matrix])
		assert(listsEqual(
			m1.data,
			IndexedSeq[Double](217).grouped(1).toIndexedSeq
		))
	}
	{//Dual Vector - Dot Product Test 2
		val v1 = new Vector((3.0 to 5.0 by 1.0), vertical = true)
		val v2 = new Vector((7.0 to 9.0 by 1.0), vertical = false)
		val m1 = v1 ** v2

		assert(m1.isInstanceOf[Matrix])
		assert(listsEqual(
			m1.data,
			IndexedSeq[Double](21, 24, 27, 28, 32, 36, 35, 40, 45).grouped(1).toIndexedSeq
		))
	}
	/*
	 * End Dual Vector Tests
	 */

	/*
	 * Matrix Scalar Tests
	 */
	{//Scalar - Add Test 1
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = m1 + 10

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](11, 12, 13, 14, 15, 16).grouped(3).toIndexedSeq
		))
	}
	{//Scalar - Add Test 2
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = 10 + m1

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](11, 12, 13, 14, 15, 16).grouped(3).toIndexedSeq
		))
	}

	{//Scalar - Subtract Test 1
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = m1 - 5

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](-4, -3, -2, -1, 0, 1).grouped(3).toIndexedSeq
		))
	}

	{//Scalar - Subtract Test 2
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = 5 - m1

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](4, 3, 2, 1, 0, -1).grouped(3).toIndexedSeq
		))
	}

	{//Scalar - Multiply Test 1
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = m1 * 2

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](2, 4, 6, 8, 10, 12).grouped(3).toIndexedSeq
		))
	}

	{//Scalar - Multiply Test 2
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = 2 * m1

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](2, 4, 6, 8, 10, 12).grouped(3).toIndexedSeq
		))
	}

	{//Scalar - Divide Test 1
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = m1 / 2

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](1/2.0, 2/2.0, 3/2.0, 4/2.0, 5/2.0, 6/2.0).grouped(3).toIndexedSeq
		))
	}

	{//Scalar - Divide Test 2
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = 2 / m1

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](2.0/1, 2.0/2, 2.0/3, 2.0/4, 2.0/5, 2.0/6).grouped(3).toIndexedSeq
		))
	}
	/*
	 * End Matrix Scalar Tests
	 */

	/*
	 * Vector Scalar Tests
	 */
	{//Scalar - Add Test 1A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = v1 + 10

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](11, 12, 13, 14, 15, 16).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Add Test 1B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = v1 + 10

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](11, 12, 13, 14, 15, 16).grouped(6).toIndexedSeq
		))
	}
	{//Scalar - Add Test 2A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = 10 + v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](11, 12, 13, 14, 15, 16).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Add Test 2B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = 10 + v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](11, 12, 13, 14, 15, 16).grouped(6).toIndexedSeq
		))
	}

	{//Scalar - Subtract Test 1A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = v1 - 5

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](-4, -3, -2, -1, 0, 1).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Subtract Test 1B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = v1 - 5

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](-4, -3, -2, -1, 0, 1).grouped(6).toIndexedSeq
		))
	}

	{//Scalar - Subtract Test 2A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = 5 - v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](4, 3, 2, 1, 0, -1).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Subtract Test 2B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = 5 - v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](4, 3, 2, 1, 0, -1).grouped(6).toIndexedSeq
		))
	}

	{//Scalar - Multiply Test 1A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = v1 * 2

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](2, 4, 6, 8, 10, 12).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Multiply Test 1B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = v1 * 2

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](2, 4, 6, 8, 10, 12).grouped(6).toIndexedSeq
		))
	}

	{//Scalar - Multiply Test 2A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = 2 * v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](2, 4, 6, 8, 10, 12).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Multiply Test 2B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = 2 * v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](2, 4, 6, 8, 10, 12).grouped(6).toIndexedSeq
		))
	}

	{//Scalar - Divide Test 1A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = v1 / 2

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](1/2.0, 2/2.0, 3/2.0, 4/2.0, 5/2.0, 6/2.0).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Divide Test 1B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = v1 / 2

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](1/2.0, 2/2.0, 3/2.0, 4/2.0, 5/2.0, 6/2.0).grouped(6).toIndexedSeq
		))
	}

	{//Scalar - Divide Test 2A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = 2 / v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](2.0/1, 2.0/2, 2.0/3, 2.0/4, 2.0/5, 2.0/6).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Divide Test 2B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = 2 / v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](2.0/1, 2.0/2, 2.0/3, 2.0/4, 2.0/5, 2.0/6).grouped(6).toIndexedSeq
		))
	}
	/*
	 * End Vector Scalar Tests
	 */

	/*
	 * Unary Tests
	 */
	{//Scalar - Negate Test 1A
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = true)
		val v2 = -v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](-1, -2, -3, -4, -5, -6).grouped(1).toIndexedSeq
		))
	}
	{//Scalar - Negate Test 1B
		val v1 = new Vector((1.0 to 6.0 by 1.0), vertical = false)
		val v2 = -v1

		assert(v2.isInstanceOf[Vector])
		assert(listsEqual(
			v2.data,
			IndexedSeq[Double](-1, -2, -3, -4, -5, -6).grouped(6).toIndexedSeq
		))
	}
	{//Scalar - Negate Test 2
		val m1 = new Matrix((1.0 to 6.0 by 1.0).grouped(3).toIndexedSeq)
		val m2 = -m1

		assert(m2.isInstanceOf[Matrix])
		assert(listsEqual(
			m2.data,
			IndexedSeq[Double](-1, -2, -3, -4, -5, -6).grouped(3).toIndexedSeq
		))
	}
	/*
	 * End Unary Tests
	 */

	println("All Tests Passed!")

	def listsEqual(l1: IndexedSeq[IndexedSeq[Double]], l2: IndexedSeq[IndexedSeq[Double]]): Boolean = {
		val numDiff = (l1.flatten zip l2.flatten).count{ case (l1_e, l2_e) => l1_e != l2_e }
		return numDiff == 0
	}
}
