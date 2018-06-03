import hw.generics._

sealed trait BinTree [A] extends ListLike[A, BinTree [A]] //A is the type of element Bintree is the Collection 

//both Node and Leaf must implement head, tail, isEmpty, (The ListLike requirements)
case class Node [A](lhs : BinTree [A], value : A, rhs : BinTree [A]) extends BinTree [A] with Product with Serializable{ //need prod and Serializable????
	//A is the generic type for the BinTree
	def cons(head: A): BinTree[A] = {
		consHelp(head, this, List[BinTree[A]]())
	}

	def consHelp(head: A, currTree: BinTree[A], parents: List[BinTree[A]]): BinTree[A] = currTree match { //find pos for new left 
		case curr: Node[A] => consHelp(head, curr.lhs, curr :: parents) //continue searching for spot
		case curr: Leaf[A] => { //new node for head and call helper to rebuild
			val newNode = Node[A](Leaf(), head, Leaf())
			rebuild(parents, newNode)
		}
	}

	def rebuild(list: List[BinTree[A]], tree: BinTree[A]): BinTree[A] = list match {//restructures tree with a list of past trees 
		case head :: tail => { 
			head match {
				case x: Node[A] => {
					val newTree = Node[A](tree, x.value, x.rhs) 
					rebuild(tail, newTree)	
				}		
				case _ => ???
			}
		}
		case Nil => tree
	}

	def head(): Option[A] = {
		findHead(this, None)
	}

	def findHead [A](tree: BinTree[A], currLeft: Option[A]): Option[A] = tree match {
		case x: Leaf[A] => currLeft
		case x: Node[A] => findHead(x.lhs, Some(x.value))
	}

	def tail(): Option[BinTree[A]] = {
		tailHelper(this, List[BinTree[A]]())
	}

	def tailHelper(currTree: BinTree[A], parents: List[BinTree[A]]): Option[BinTree[A]] = currTree match { 
		case curr: Node[A] => { //check more left 
			curr.lhs match {
				case left: Leaf[A] => { //now check the right 
					curr.rhs match {
						case right: Leaf[A] => { //drop the whole currTree because it is head
							Some(rebuild(parents, Leaf()))
						}
						case right: Node[A] => { //preserve the nodes on the right (become new left)
							Some(rebuild(parents, right))
						}
					}
				}
				case left: Node[A] => { //currTree is not the head, has left node
					tailHelper(left, curr :: parents)
				}
			}
		}
		case curr: Leaf[A] => { //new node for head and call helper to rebuild
			None //rebuild(parents, Leaf[A])//Should this be NONE?
		}
	}

	def isEmpty(): Boolean = (this.head == None)
}

case class Leaf [A]() extends BinTree [A] {
	def cons(head: A): BinTree[A] = {
		val node = Node[A](Leaf[A](), head, Leaf[A])
		node
	}
	
	def head(): Option[A] = None

	def tail(): Option[BinTree[A]] = None

	def isEmpty(): Boolean = true
}




object ListFunctions {
	def filter[A, C <: ListLike[A, C]](f: A => Boolean, alist: C): C = {
		val emptied = emptyListLike[A, C](alist)
		val filteredReved = filtHelp(f, alist, emptied)
		reverse[A, C](filteredReved, emptyListLike[A, C](filteredReved))
	}

	def emptyListLike[A, C <: ListLike[A, C]](alist: C): C = {
		alist.tail match {
			case Some(y) => {
				emptyListLike[A,C](y)
			}
			case None => {
				alist
			}
		}
	}

	def filtHelp[A, C <: ListLike[A, C]](f: A => Boolean, alist: C, alist2: C): C = {
		if (!alist.isEmpty){
			alist.tail match {
				case Some(x) => {
					alist.head match {
						case Some(y)  => {
							if (f(y) == true) {
								filtHelp(f, x, alist2.cons(y))
							}
							else filtHelp(f, x, alist2)
						}
						case _ => ???
					}
				}
				case None => alist2
			}
		}
		else alist2
	}

	def reverse[A, C <: ListLike[A, C]](alist: C, ret: C): C = { 
		alist.head match {
			case Some(x) => {
				alist.tail match {
					case Some(y) => reverse[A, C](y, ret.cons(x))
					case None => ret.cons(x)
				}
			}
			case None => ret
		}
	}


	def append[A, C <: ListLike[A, C]](alist1: C, alist2: C): C = { //create a backwards listlike, then a forwards one by reving???
		val emptied = emptyListLike[A, C](alist1)
		val reved = appHelp[A, C](alist1, alist2, emptied)
		reverse[A, C](reved, emptyListLike[A, C](reved))
	}

	def appHelp[A, C <: ListLike[A, C]](alist1: C, alist2: C, comb: C): C = { //.head on empty? 
		if (!alist1.isEmpty){		
			alist1.head match {
				case Some(x) => {
					alist1.tail match {
						case Some(y) => appHelp[A, C](y, alist2, comb.cons(x))
						case None => ???
					}
				}
				case None => appHelp[A, C](alist1, alist2, comb)
			}
		}
		else {
			alist2.head match {
				case Some(x) => {
					alist2.tail match {
						case Some(y) => appHelp[A, C](alist1, y, comb.cons(x))
						case None => comb.cons(x)
					}
				}
				case None => comb
			}
		}
	}

	def insert[A <: Ordered [A], C <: ListLike [A, C]](elem: A, alist: C, ordered: List[A]): C = { //ordered is the list of ordered heads in reverse (decending which is right)
		alist.head match {
			case Some(x) => {
				if (elem.compare(x) == LT) build[A, C](alist.cons(elem), ordered)
				else if (elem.compare(x) == EQ) build[A, C](alist.cons(elem), ordered)
				else {
					alist.tail match {
						case Some(y) => insert[A, C](elem, y, x :: ordered)
						case None => build[A, C](alist, (elem :: ordered)) //problem?
					}
				}
			}
			case None => build[A, C](alist.cons(elem), ordered) //problem???
		}
	}


	def build[A <: Ordered[A], C <: ListLike[A,C]] (curr: C, pastHeads: List[A]): C = pastHeads match {//restructures tree with a list of past trees 
		case head :: tail => { 
			head match {
				case Nil => curr
				case _ => build[A, C](curr.cons(head), tail)		
			}
		}
		case Nil => curr
	}


	def sort [A <: Ordered [A], C <: ListLike [A, C]](alist : C): C = {
		sortHelp[A, C](alist, emptyListLike[A, C](alist))
	}

	def sortHelp[A <: Ordered [A], C <: ListLike [A, C]](alist: C, ret: C): C = {
		alist.head match {
			case Some(x) => {
				alist.tail match {
					case Some(y) => sortHelp[A, C](y, insert(x, ret, List[A]()))
					case None => insert(x, ret, List[A]())
				}
			}
			case None => ret
		}
	}
}

class C1 extends T2[Int, Int, String, String] with T3[Int, Int, Int, String, String, String, Int]{
	// Do not change the class body. Simply extend T1, T2, and/or T3.
	def f(a: Int, b : Int): Int = 0
	def g(c: String): String = ""
	def h(d: String): Int = 0
}

class C2 extends T1[Int, Int] with T2[Int, Int, Int, Int] with T3[Int, Int, Int, Int, Int, Int, Int]{
	// Do not change the class body . Simply extend T1 , T2 , and/or T3.
	def f(a: Int, b : Int): Int = 0
	def g(c: Int): Int = 0
	def h(d: Int): Int = 0
}

class C3 [A](x : A) extends T3[Int, A, Int, A, String, String, A]{
	// Do not change the class body. Simply extend T1, T2, and/or T3.
	def f(a: Int, b : A): Int = 0
	def g(c: A): String = ""
	def h(d: String): A = x
}

class C4 [A](x : Int, y: C4 [A]) extends T1[Int, C4[A]] with T3[Int, C4[A], C4[A], Int, C4[A], C4[A], Int]{
	// Do not change the class body. Simply extend T1, T2, and/or T3.
	def f(a: Int, b : C4 [A]): C4 [A] = b
	def g(c: Int): C4 [A] = y
	def h(d: C4 [A]): Int = x
}


/*
Discussion:
Integer[] myarray = new Integer[]{1};

void reKT(Object[] ma){
	ma[o] = "get rekt";
}

get type check exception if pass integer array into reKT (compiles but does not run since Java is covariant)
(Java is covariant) (can shove subtypes into function) (have a parent and any children can go in)
Scala is invariant, can only put in that type, cannot put in the children (Object array is not Integer array)
May not give child in invariant (will fail the typecheck at compile time)


ListLike Object need to parameterize the functions and make say c and make it subtype of ListLike (c <: ListLike)
Methods must be available on all of the types that are subtypes of the parent! Cannot use methods specific to children, must use ones parents have
Must do C <: ListLike to basically override the typecheck? make it covariant


class Top
class Middle extends top
class Bottom extends middle
class Something[+A]{} is covariant (The +A is a promise you wont implement set or get methods)
	def foo(x: Something[Middle]): x 
}

foo(new Something[Bottom]) //works because subtype of middle Bottom <: Middle then Something[Bottom] <: Something[Middle]
foo(new Something[Top]) // fails at compile time because type check (Top is not child of middle, it is a super type) Middle<: Top

covariance object and down to children C <: Foo      (Colon means that itself is included)  [+A] in Scala
invariance is only object           Foo
contravariance includes object and parents all the way up C >: Foo  (Colon means that itself is included)  [-A] in Scala

define type recursively (for sort     C is in List[A, C (Which is ListLike)]) (Define it then recursively define it)
C<: ListLike[A, C] (A covariant of ORder and C is covariant of ListLike)
List[Int] <: ListLike[Int, List[Int]]}


which types does it work to extend? 

Can just type in extensions and see which ones fail then romove them 

A implements Ordered so can use it for sorting or ordering functions 
Tests on multiple types
ListLike really isnt list, so have to redefine methods


sealed traits vs traits: 
sealed traits know all of the possible types (sealed and closed off do not allow you to extend) (no animals in vegetables)
traits allow you to extend (can add random stuff) (No exhaustivity checks) (could add animals to vegetables)
sealed is kind of like case class
sealed Bin extends listLike (Nodes and leaves) (only need to check if node or leave)

ListLike has 4 different 

List[Int] <: ListLike[Int, List[Int]]}
implements member of ListLike 
*/
