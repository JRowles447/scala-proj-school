import HOF._
class TestSuite extends org . scalatest . FunSuite {
	
	//Exercise 1
	test ("map2 with functions") {
		def add (x : Int , y: Int ): Int = x + y
		def sub (x : Int , y: Int ): Int = x - y
		def mult (x : Int , y: Int ): Int = x * y
		assert (map2 (add , List (1, 2, 3) , List (4, 5, 6)) == List (5, 7, 9))
		assert (map2 (add, List(), List()) == List())
		assert (map2 (sub, List(2, 3, 4), List (1, 2, 3)) == List(1, 1, 1))
		assert (map2 (mult, List(1, 2, 3, 4, 5), List(0, 3, 4, 5, 2)) == List(0, 6, 12, 20, 10))
		assert (map2(add, List(1, 2, 3, 4), List (5, 6)) == List(6, 8))
	}

	test ("zip tests") {
		assert ( zip ( List (1 , 2, 3) , List (4 , 5, 6)) == List ((1, 4), (2, 5), (3, 6)))
		assert ( zip ( List ( " George " , " Teddy " ), List ( " Washington " , " Roosevelt " )) ==
						List (( " George " , " Washington " ), ( " Teddy " , " Roosevelt " )))
		assert (zip(List(2), List(5)) == List((2, 5)))
		assert (zip(List(), List()) == List())
	}

	//Exercise 3 apply map2 to the flatten function
	test ("flatten test" ) {
		assert ( flatten ( List ( List (1 , 2) , List (3 , 4))) == List (1 , 2, 3, 4))
		assert (flatten (List(List(2, 3, 4), List(1, 2, 3), List(0, 0, 0))) == List(2, 3, 4, 1, 2, 3, 0, 0, 0))
		assert (flatten (List(List(), List())) == List())
		assert (flatten (List(List())) == List())
		assert (flatten (List(List(), List(1), List(2, 3, 4))) == List(1, 2, 3, 4))
		assert (flatten (List(List(4, -1), List(), List(5, 2))) == List(4, -1, 5, 2))
	}

	//Exercise 4 
	test ("flatten3 test"){
		assert (flatten3(List(List(List(1, 2), List(3, 4)), List(List(5, 6), List(7, 8)))) == List(1, 2, 3, 4, 5, 6, 7, 8))
		assert (flatten3(List(List(List()))) == List())
		assert (flatten3(List(List(List(1), List()), List(List(2, 3)), List(List(4, 5, 6)))) == List(1, 2, 3, 4, 5, 6))
	}

	//Exercise 5 
	test ("buildList test working correctly"){
		def f(x: Int) = x
		def addNum(x : Int) = x + 5
		def mult(x : Int) = x * 2
		def sub (x : Int) = x - 10
		assert (buildList (10 , f) == List (0 , 1, 2, 3, 4, 5, 6, 7, 8, 9))
		assert (buildList(5, addNum) == List (5, 6, 7, 8, 9))
		assert (buildList(0, addNum) == List ()) //list of zero returns empty because does not include 0
		assert (buildList(5, mult) == List (0, 2, 4, 6, 8))
		assert (buildList(1, addNum) == List (5))
		assert (buildList(5, sub) == List (-10, -9, -8, -7, -6))
		assert (buildList(-3, mult) == List())
	}

	//Exercise 6
	test ("mapList test") {
		def f(n: Int): List [Int] = buildList (n, (_: Int) => n)
		def func(n: Int): List[Int] = List(n)
		assert (mapList (List (1 , 2 , 3), f) == List (1 , 2, 2 , 3 , 3 , 3))
		assert (mapList (List(1, 2, 3, 4), f) == List(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
		assert (mapList (List(), f) == List())
		assert (mapList (List(0, -1), f) == List())
		assert (mapList (List(-2, -1, 0, 1, 2, 3), func) == List(-2, -1, 0, 1, 2, 3))
	}

	//Append test
	test ("append working correctly") {
		val lst1 = List(1, 2, 3) 
		val lst2 = List(3, 4, 5)
		val emp = List()
		val one = List(1)
		val long = List(1, 2, 3, 4, 5, 6, 7)
		assert (append(lst1, lst2) == List(1, 2, 3, 3, 4, 5))
		assert (append(emp, emp) == List())
		assert (append(lst1, lst1) == List(1, 2, 3, 1, 2, 3))
		assert (append(one, long) == List(1, 1, 2, 3, 4, 5, 6, 7))
		assert (append(long, one) == List(1, 2, 3, 4, 5, 6, 7, 1))
		assert (append(emp, long) == long)
		assert (append(long, emp) == long)
	}

	def isEven (x: Int ): Boolean = x % 2 == 0
	test ("partition test 1 ") {
		assert ( partition ( isEven , List (1 ,2 ,3 ,4 ,5 ,6)) == ( List (2 ,4 ,6) , List (1 ,3 ,5)))
		assert ( partition ( isEven , List (2 ,4 ,6)) == ( List (2 ,4 ,6) , Nil ))
		assert ( partition ( isEven , List (1 ,3 ,5)) == ( Nil , List (1 ,3 ,5)))
		}

	//Exercise 8
	def lt (x : Int , y: Int ): Boolean = x < y
	test ("Merge working correctly ") {
		assert (merge (lt , List (5 , 3, 1) , List (10 , 6, 0)) == List (10 , 6, 5, 3, 1, 0))
		assert (merge (lt, List(), List(5, 4, -1)) == List(5, 4, -1))
		assert (merge (lt, List(5, 4, -1), List()) == List(5, 4, -1))
		assert (merge (lt, List(1), List(1)) == List(1, 1))
		assert (merge (lt, List(), List()) == List())
		assert (merge (lt, List(-1), List(10, 4, 3, -2, -3)) == List(10, 4, 3, -1, -2, -3))
		assert (merge (lt, List(-1), List(10, 4, 3)) == List(10, 4, 3, -1)) 
	}

	// //Exercise 9
	test("sort working correctly") {
		def lessThan(x : Int, y : Int) : Boolean = x < y
		assert (sort (lessThan , List (5 ,1 ,2 ,3 ,4 ,5)) == List (5 ,5 ,4 ,3 ,2 ,1))
		assert (sort (lessThan, List()) == List())
		assert (sort (lessThan, List(-5, -2, 0, 3, 6, 8)) == List(8, 6, 3, 0, -2, -5))
		assert (sort (lessThan, List(-1)) == List(-1))
		assert (sort (lessThan, List(2, 5)) == List(5, 2))
		assert (sort (lessThan, List(5, 2)) == List(5, 2))
	}

}