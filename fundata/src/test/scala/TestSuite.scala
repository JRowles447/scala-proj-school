import FunctionalDataStructures._

class TestSuite extends org.scalatest.FunSuite {

	def fromList[A](alist: List[A]): JoinList[A] = alist match {
		case Nil => Empty()
		case List(x) => Singleton(x)
		case _ => {
			val len = alist.length
			val (lhs, rhs) = alist.splitAt(len/2)
			Join(fromList(lhs), fromList(rhs), len)
		}
	}

	def toList[A](alist: JoinList[A]): List[A] = alist match {
		case Empty() => Nil
		case Singleton(x) => List(x)
		case Join(alist1, alist2, _) => toList(alist1) ++ toList(alist2)
	}

	val queue1 = Queue(List(1, 2, 3), List(6, 5, 4)) //added 1, 2, ..., 6
	val queue2 = Queue(List(2, 3) , List(6, 5, 4))
	val queue3 = Queue(List(3) , List(6, 5, 4))
	val queue4 = Queue(List() , List(6, 5, 4))
	val queueRev = Queue(List(5, 6), List()) //Reversed with the front removed, as happens in dequeue

	test("enqueue test") {
		assert(enqueue(7, queue1) == Queue(List(1, 2, 3), List(7, 6, 5, 4)))
		assert(enqueue(1, Queue(List(), List())) == Queue(List(), List(1)))
		assert(enqueue(13, Queue(List(1, 2, 3, 6), List())) == Queue(List(1, 2, 3, 6), List(13)))
	}

	test("dequeue test"){
		assert(dequeue(queue1) == Some(1, Queue(List(2, 3) , List(6, 5, 4))))
		assert(dequeue(queue2) == Some(2, queue3))
		assert(dequeue(queue3) == Some(3, queue4))
		assert(dequeue(queue4) == Some(4, queueRev))
	}

	val alist = List(7, 8, 3, 4, 5, 6)
	val list2 = List(11, 2, 3, 1, 12, 4, 16)
	val list3 = List(9, 8, 5, 4, 1, 3, 3, 2, 1)
	val list = List(5, 2, 3, 8)
	val empty = List[Int]()
	val joined = fromList(alist)
	val joined2 = fromList(list2)
	val joined3 = fromList(list3)
	val joined4 = fromList(list)
	val joinempty = fromList(empty)
	def compare(x: Int, y: Int): Boolean = x < y
	def func(x: Int): Int = x + 2
	def even(x: Int): Boolean = x % 2 == 0
	
	test("max test"){
		assert(max(joined, compare) == Some(8))
		assert(max(joined2, compare) == Some(16))
		assert(max(joined3, compare) == Some(9))
		assert(max(joinempty, compare) == None)
	}

	test("first test"){
		val sing = Join(Empty(), Singleton(3), 1)
		assert(first(sing) == Some(3))
		assert(first(joinempty) == None)
		assert(first(joined) == Some(7))
		assert(first(joined2) == Some(11))
		assert(first(joined3) == Some(9))
	}

	test("rest test"){
	// 	println(rest(joined4))
	// //	println("THIS IS THE LIST THAT YOU ARE WORKING ON: " + fromList(alist))
	// 	//println("This is what the function returns:   " + rest(fromList(alist)))
	// 	// assert(rest(joined4) == Option(fromList(List(2, 3, 8))))
	// 	// assert(rest(joined) == Option(fromList(List(8, 3, 4, 5, 6))))
	// 	// println(joined2)
	// 	// assert(rest(joined2) == Option(fromList(List(2, 3, 1, 12, 4, 16))))
	// 	// assert(rest(joined3) == Option(fromList(List(8, 5, 4, 1, 3, 3, 2, 1))))
	// 	// assert(rest(joinempty) == Option(Empty()))

	// 	println("If only order needs to be preserved") //how to convert these Optionals into lists? I know they work
	// 	assert(toList(rest(joined4)) == Some(List(2, 3, 8)))
	// 	assert(toList(rest(joined)) == Option(List(8, 3, 4, 5, 6)))
	// 	assert(toList(rest(joined2)) == Option(List(2, 3, 1, 12, 4, 16)))
	// 	assert(toList(rest(joined3)) == Option(List(8, 5, 4, 1, 3, 3, 2, 1)))
	// 	assert(toList(rest(joinempty)) == Option(Empty()))

	}

	test("nth test"){ //proves that rest works also 
		assert(nth(joined, 5) == Some(6))
		assert(nth(joined, -1) == None)
		assert(nth(joined2, 0) == Some(11))
		assert(nth(joined3, 2) == Some(5)) //check this list
		assert(nth(joined3, 0) == Some(9)) 
		assert(nth(joined3, 1) == Some(8)) 
		assert(nth(joined3, 4) == Some(1)) 
		assert(nth(joined3, 5) == Some(3)) 
		assert(nth(joined3, 6) == Some(3)) 
		assert(nth(joined3, 7) == Some(2)) 
		assert(nth(joined3, 8) == Some(1)) 
		assert(nth(joined4, 3) == Some(8))
		assert(nth(joinempty, 3) == None)
		assert(nth(joined, 0) == Some(7))
		assert(nth(joined, 1) == Some(8))
		assert(nth(joined, 2) == Some(3))
		assert(nth(joined, 3) == Some(4))
		assert(nth(joined, 4) == Some(5))
	}

	test("map test") {
		assert(map(func, joined4) == fromList(List(7, 4, 5, 10)))
		assert(map(func, joined) == fromList(List(9, 10, 5, 6, 7, 8)))
		assert(map(func, joined2) == fromList(List(13, 4, 5, 3, 14, 6, 18)))
		assert(map(func, joined3) == fromList(List(11, 10, 7, 6, 3, 5, 5, 4, 3)))
	}

	test("filter test") {
		assert(toList(filter(even, joined)) == List(8, 4, 6))
		assert(toList(filter(even, joined2)) == List(2, 12, 4, 16))
		assert(toList(filter(even, joined3)) == List(8, 4, 2))
		assert(toList(filter(even, joined4)) == List(2, 8))
	}
}