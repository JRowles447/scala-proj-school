import Lists ._
class TestSuite extends org . scalatest . FunSuite {
	test ("oddNumbers properly defined ") {
	assert ( oddNumbers == List (1 , 3 , 5))
	}

	//Test 2.1
	test ("SumDouble working correctly"){
		val check = List(1 , 2 , 3)
		val neg = List(-1, -4, -7)
		val check2 = List()
		val check3 = List(0)
		val check4 = List(5, 7, 2, 3, 1)
		val check5 = List(3, 9, 0, 1)
		val negPos = List(-1, 5, -6, 3)
		assert (sumDouble(negPos) == 2)
		assert (sumDouble(neg) == -24)
		assert (sumDouble(check) == 12)
		assert (sumDouble(check2) == 0)
		assert (sumDouble(check3) == 0)
		assert (sumDouble(check4) == 36)
		assert (sumDouble(check5) == 26)
	}

	//Test 2.2
	test ("removeZeroes working"){
		val check = List(1 , 0 , 3, 5, 8)
		val removed = List(1 , 3, 5, 8) 
		val list1 = List(0, 1, 5, 0, 2, 0, 0)
		val empty = List()
		val zeroList = List(0, 0, 0, 0, 0, 0)
		assert (removeZeros(check) == removed)
		assert (removeZeros(list1) == List(1, 5, 2))
		assert (removeZeros(empty) == List())
		assert (removeZeros(zeroList) == List())
	}

	//Test 2.3
	test ("countEvens working correctly"){
		val check = List(1 , 2 , 3, 4)
		val check2 = List(1, 3, 5, 7, 111, 33)
		val check3 = List(0 , 2 , 3, 4, 5, 6, 11, 33, 4)
		val check4 = List()
		assert (countEvens(check) == 2)
		assert (countEvens(check2) == 0)
		assert (countEvens(check3) == 5)
		assert (countEvens(check4) == 0)
	}

	//Test 2.4 
	test ("removeAlternating working correctly"){
		val orig = List("a", "b", "c", "d", "e")
		val list = List("a", "c", "d", "e")
		val wow = List("a")
		val longList = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
		assert (removeAlternating(orig) == List("a", "c", "e"))
		assert (removeAlternating(list) == List("a", "d"))
		assert (removeAlternating(wow) == List("a"))
		assert (removeAlternating(longList) == List("a", "c", "e", "g", "i", "k"))
	}

	//Test 2.5 
	test ("isAscending working correctly"){
		val ascend = List(1, 1, 3, 6, 7, 8, 91)
		val ascendList = List(-31, -12, -12, 0, 1, 1, 3, 6, 7, 8, 91)
		val notAscend = List(3, 4, 12, 8)
		val notAscendList = List(13, -12, 4)
		val emptyList = List()
		val break = List(4, 5, 1)
		assert (isAscending(ascend))
		assert (isAscending(ascendList))
		assert (!isAscending(notAscend))
		assert (!isAscending(notAscendList))
		assert (isAscending(emptyList))
		assert (!isAscending(break))
	}

	//Test 2.6
	test ("addSub working correctly"){
		val list1 = List(10, 20, 30, 40)
		val list2 = List(1, 2)
		val list3 = List(1, 2, 3)
		val list4 = List(4)
		val list5 = List(4, -5, -2, 7, -1)
		val empty = List()
		assert (addSub(list1) == -20)
		assert (addSub(list2) == -1)
		assert (addSub(list3) == 2)
		assert (addSub(list4) == 4)
		assert (addSub(list5) == -1)
		assert (addSub(empty) == 0)
	}

	//Test 2.7
	test ("alternate working correctly"){
		val list1 = List(10, 20, 30, 40)
		val list2 = List(99, 88, 87, 86)
		val short1 = List(1)
		val short2 = List(2)
		val long1 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2)
		val long2 = List(11, 22, 33, 44, 55, 66, 77, 88, 99, 11, 22)
		val empty1 = List()
		val empty2 = List()
		val duplicate = List(1, 2, 3)
		val merged = alternate(list1, list2)
		val merged2 = alternate(short1, short2)
		val merged3 = alternate(long1, long2)
		val merged4 = alternate(empty1, empty2)
		val mergedDup = alternate(duplicate, duplicate)
		assert (merged == List (10, 99, 20, 88, 30, 87, 40, 86))
		assert (merged2 == List(1, 2))
		assert (merged3 == List(1, 11, 2, 22, 3, 33, 4, 44, 5, 55, 6, 66, 7, 77, 8, 88, 9, 99, 1, 11, 2, 22))
		assert (merged4 == List())
		assert (mergedDup == List(1, 1, 2, 2, 3, 3))
	}

	//Exercise 2.8
	test ("fromTo working correctly"){
		assert(fromTo(1, 6) == List(1, 2, 3, 4, 5))
		assert(fromTo(-9, 0) == List (-9, -8, -7, -6, -5, -4, -3, -2, -1))
		assert(fromTo(-2, 1) == List(-2, -1, 0))
		assert(fromTo(-50, -49) == List(-50))
	}

	//Exercise 2.9
	test ("insertOrdered working correctly"){
		assert(insertOrdered(3, List(1, 5, 7, 9)) == List(1, 3, 5, 7, 9))
		assert(insertOrdered(3, List (1, 2, 3, 3, 5)) == List(1, 2, 3, 3, 3, 5))
		assert(insertOrdered(6, List(1, 5, 7, 9)) == List(1, 5, 6, 7, 9))
		assert(insertOrdered(30, List(1, 5, 7, 9)) == List(1, 5, 7, 9, 30))
		assert(insertOrdered(0, List(1, 5, 7, 9)) == List(0, 1, 5, 7, 9))
		assert(insertOrdered(-50, List(1, 5, 7, 9)) == List(-50, 1, 5, 7, 9))
		assert(insertOrdered(3, List()) == List(3))
		assert(insertOrdered(5, List(3)) == List(3, 5))
		assert(insertOrdered(-1, List(0)) == List(-1, 0))
		assert(insertOrdered(-10, List()) == List(-10))
		assert(insertOrdered(0, List()) == List(0))
	}

	//Exercise 2.10
	test ("sort working correctly"){
		val mess = List(3, 4, 1, 8, 3, 10)
		val empty = List ()
		val orderedRep = List(1, 2, 3, 7, 7, 9)
		val descending = List(10, 6, 3, 2, 1, 0, -34)
		val list = List(1)
		val negs = List (-10, -4, -15, -8, 0, -16)
		assert (sort(mess) == List(1, 3, 3, 4, 8, 10))
		assert (sort(empty) == List())
		assert (sort(orderedRep) == List(1, 2, 3, 7, 7, 9))
		assert (sort(descending) == List(-34, 0, 1, 2, 3, 6, 10))
		assert (sort(list) == List(1))
		assert (sort(negs) == List(-16, -15, -10, -8, -4, 0))
	}
}