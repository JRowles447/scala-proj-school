import ListFunctions._
import hw.generics._
import hw.generics.MyList

class TestSuite extends org.scalatest.FunSuite {

	val tree1 = Node[Int](Node[Int](Leaf[Int](), 0, Leaf[Int]()), 1, Node[Int](Leaf[Int](), 2, Leaf[Int]()))
	val tree2 = Node[Int](Leaf() , 1, Node(Leaf(), 2, Leaf()))
	val root = Node[Int](Leaf(), 2, Leaf())
	val largeNum = Node[Int](Leaf(), 63, Leaf())
	val empty = Leaf[Int]()
	val treeStr1 = Node[String](Node(Node(Leaf(), "hello", Leaf()), "world", Leaf()), "goodbye", Leaf())
	val treeStr2 = Node[String](Node(Leaf(), "world", Leaf()), "goodbye", Leaf())
	val treeStr3 = Node[String](Leaf(), "goodbye", Leaf())
	val myList1 = Cons[Int](6, Cons[Int](7, Cons[Int](8, Empty[Int]())))
	val myList2 = Cons[Int](7, Cons[Int](8, Empty[Int]()))
	val myList3 = Cons[Int](8, Empty[Int]())
	val longList = Cons[Int](9, Cons[Int](4, Cons[Int](13, Cons[Int](6, Cons[Int](16, Empty[Int]())))))
	val myListStr1 = Cons[String]("hello", Cons[String]("world", Empty[String]()))
	val myListStr2 = Cons[String]("world", Empty[String]()) 
	val listIntLikes = Cons[IntLike](IntLike(9), Cons[IntLike](IntLike(4), Cons[IntLike](IntLike(13), Cons[IntLike](IntLike(6), Cons[IntLike](IntLike(16), Empty[IntLike]())))))
	val treeStrLike1 = Node[StringLike](Node[StringLike](Node[StringLike](Leaf[StringLike](), StringLike("hello"), Leaf[StringLike]()), StringLike("world"), Leaf[StringLike]()), StringLike("goodbye"), Leaf[StringLike]())
	val listIntLikes2 = Cons[IntLike](IntLike(20), Cons[IntLike](IntLike(1), Cons[IntLike](IntLike(3), Cons[IntLike](IntLike(-4), Cons[IntLike](IntLike(5), Empty[IntLike]())))))


	case class IntLike(x: Int) extends Ordered[IntLike]{
		def compare(y: IntLike): Ordering = {
			if(this.x.compare(y.x) < 0) LT
			else if (this.x.compare(y.x) == 0) EQ
			else GT
		}
	}

	case class StringLike(x: String) extends Ordered[StringLike]{
		def compare(y: StringLike): Ordering = {
			if (this.x.compare(y.x) < 0) LT
			else if (this.x.compare(y.x) == 0) EQ
			else GT
		}
	}

	test("Program works"){
		
	}

	test("binTree tests head"){
		assert(tree1.head == Some(0))
		assert(tree2.head == Some(1)) 
		assert(root.head == Some(2))
		assert(empty.head == None)
		assert(treeStr1.head == Some("hello"))
		assert(treeStr2.head == Some("world"))
		assert(treeStr3.head == Some("goodbye"))
	}

	test("myList tests head"){
		assert(myList1.head == Some(6))
		assert(myList2.head == Some(7))
		assert(myList3.head == Some(8))
	}

	test("binTree tail tests"){
		assert(tree1.tail == Some(Node(Leaf() , 1, Node(Leaf(), 2, Leaf()))))
		assert(tree2.tail == Some(root))
		assert(root.tail == Some(Leaf()))
		assert(empty.tail == None)
		assert(treeStr1.tail == Some(treeStr2))
		assert(treeStr2.tail == Some(treeStr3))
		assert(treeStr3.tail == Some(Leaf()))
	}

	test("myList tests tail"){
		assert(myList1.tail == Some(myList2))
		assert(myList2.tail == Some(myList3))
		assert(myList3.tail == Some(Empty[Int]()))
	}

	test("binTree test cons") {
		assert(empty.cons(2) == root)
		assert(empty.cons(2).cons(1) == Node(Node(Leaf(), 1, Leaf()), 2, Leaf()))
		assert(empty.cons(2).cons(1).cons(0) == Node(Node(Node(Leaf(), 0, Leaf()), 1, Leaf()), 2, Leaf()))
		assert(empty.cons(2).cons(1).cons(0).head == Some(0))
		assert(empty.cons(2).cons(1).cons(0).tail == Some(empty.cons(2).cons(1)))
		//assert(treeStr1.cons("program").cons("first").cons("Your") == Node(Node(Node(Leaf(), "Your", Leaf()), "first", Leaf()), "program", Leaf()), "hello", treeStr2))
		//Fix this hot mess ^^^
	}

	test("binTree isEmpty test"){
		assert(tree1.isEmpty == false)
		assert(tree2.isEmpty == false)
		assert(root.isEmpty == false)
		assert(empty.isEmpty == true)
		assert(treeStr1.isEmpty == false)
		assert(treeStr2.isEmpty == false)
		assert(treeStr3.isEmpty == false)

	}

	def funcInt(x: Int): Boolean = (x % 2 == 0)
	def odds(x: Int): Boolean = (x % 2 == 1)
	def length(x: String): Boolean = (x.length == 5)
	def length1(x: String): Boolean = (x.length == 1)
	//def evenIntLike(x: IntLike): Boolean = (x % 2 == 0) 

	test("filter test"){
		assert(filter[Int, BinTree[Int]](funcInt, tree1) == Node[Int](Node[Int](Leaf[Int](), 0, Leaf[Int]()), 2, Leaf[Int]()))
		assert(filter[Int, BinTree[Int]](funcInt, tree2) == Node(Leaf(), 2, Leaf()))
		assert(filter[Int, BinTree[Int]](odds, tree1) == Node[Int](Leaf(), 1, Leaf()))
		assert(filter[String, BinTree[String]](length, treeStr1) == Node[String](Node[String](Leaf(), "hello", Leaf()), "world", Leaf()))
		assert(filter[String, BinTree[String]](length1, treeStr1) == Leaf[String]())
		assert(filter[Int, MyList[Int]](funcInt, myList1) == Cons[Int](6, Cons[Int](8, Empty[Int]())))
		assert(filter[Int, MyList[Int]](funcInt, myList2) == Cons[Int](8, Empty[Int]()))
		assert(filter[Int, MyList[Int]](odds, myList2) == Cons[Int](7, Empty[Int]()))
		assert(filter[Int, MyList[Int]](odds, myList3) == Empty[Int]())
		assert(filter[Int, MyList[Int]](funcInt, longList) == Cons[Int](4, Cons[Int](6, Cons[Int](16, Empty[Int]()))))
		assert(filter[Int, BinTree[Int]](odds, empty) == empty)
		//assert(filter[IntLike, MyList[IntLike]](listIntLikes) == Cons[IntLike](IntLike(4), Cons[IntLike](IntLike(6), Cons[IntLike](IntLike(16), Empty[IntLike]()))))
	}

	test("append test"){
		assert(append[Int, BinTree[Int]](root, root) == Node[Int](Node[Int](Leaf(), 2, Leaf()), 2, Leaf()))
		assert(append[Int, BinTree[Int]](tree2, tree2) == Node[Int](Node[Int](Node[Int](Node[Int](Leaf[Int](), 1, Leaf[Int]()), 2, Leaf[Int]()), 1, Leaf[Int]()), 2, Leaf[Int]()))
		assert(append[Int, BinTree[Int]](largeNum, tree2) == Node[Int](Node[Int](Node[Int](Leaf(), 63, Leaf()), 1, Leaf()), 2, Leaf()))
		assert(append[Int, BinTree[Int]](tree2, largeNum) == Node[Int](Node[Int](Node[Int](Leaf(), 1, Leaf()), 2, Leaf()), 63, Leaf()))
		assert(append[Int, BinTree[Int]](tree1, tree1) == Node[Int](Node[Int](Node[Int](Node[Int](Node[Int](Node[Int](Leaf[Int](), 0, Leaf[Int]()), 1, Leaf[Int]()), 2, Leaf[Int]()), 0, Leaf[Int]()), 1, Leaf[Int]()), 2, Leaf[Int]()))
		assert(append[Int, MyList[Int]](longList, myList3) == Cons[Int](9, Cons[Int](4, Cons[Int](13, Cons[Int](6, Cons[Int](16, Cons[Int](8, Empty[Int]())))))))
		assert(append[String, MyList[String]](myListStr1, myListStr2) == Cons[String]("hello", Cons[String]("world", Cons[String]("world", Empty[String]()))))
		assert(append[Int, BinTree[Int]](empty, empty) == empty)
		//assert(append[Int, BinTree[Int]](empty, tree2) == tree2)
	}

	val emptyIntLike = Leaf[IntLike]()
	val tree1IntLike = Node[IntLike](Node[IntLike](Node[IntLike](Leaf[IntLike](), IntLike(0), Leaf[IntLike]()), IntLike(1), Leaf[IntLike]()), IntLike(2), Leaf[IntLike]())

	test("sort test"){
		assert(sort[IntLike, MyList[IntLike]](listIntLikes) == Cons[IntLike](IntLike(4), Cons[IntLike](IntLike(6), Cons[IntLike](IntLike(9), Cons[IntLike](IntLike(13), Cons[IntLike](IntLike(16), Empty[IntLike]()))))))
		assert(sort[StringLike, BinTree[StringLike]](treeStrLike1) == Node[StringLike](Node[StringLike](Node[StringLike](Leaf[StringLike](), StringLike("goodbye"), Leaf[StringLike]()), StringLike("hello"), Leaf[StringLike]()), StringLike("world"), Leaf[StringLike]()))
		assert(sort[IntLike, MyList[IntLike]](listIntLikes2) == Cons[IntLike](IntLike(-4), Cons[IntLike](IntLike(1), Cons[IntLike](IntLike(3), Cons[IntLike](IntLike(5), Cons[IntLike](IntLike(20), Empty[IntLike]()))))))
		assert(sort[IntLike, BinTree[IntLike]](emptyIntLike) == emptyIntLike)
		assert(sort[IntLike, BinTree[IntLike]](tree1IntLike) == tree1IntLike)
	}
	//	val treeStr1 = Node[String](Node(Node(Leaf(), "hello", Leaf()), "world", Leaf()), "goodbye", Leaf())

}