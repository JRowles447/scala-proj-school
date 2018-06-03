object FunctionalDataStructures {
	
	case class Queue [A](front : List [A], back : List [A])
	
	def enqueue [A](elt: A, q: Queue [A]): Queue [A] = q.back match {
		case List() => Queue[A](q.front, List(elt))
		case _ => Queue[A](q.front, elt :: q.back)
	}
	
	def dequeue [A](q: Queue [A]): Option [(A, Queue [A])] = (q.front, q.back) match {
		case (head :: tail, _) => Option(head, Queue[A](tail, q.back))
		case (Nil, _) => {
			val replace = q.back.reverse
			replace match { //Checks the new reversed back (now the front) and returns the element at front
				case(head :: tail) => Option(head, Queue[A](tail, List()))
				case(Nil) => None //Not going to work? But what do we do if dequeue on an Empty queue???
			}

		}
	}
	
	sealed trait JoinList [A] {
		val size : Int
	}

	case class Empty [A]() extends JoinList [A] {
		val size = 0
	}

	case class Singleton [A](elt: A) extends JoinList [A] {
		val size = 1
	}

	case class Join [A](lst1: JoinList [A], lst2: JoinList [A] , size : Int) extends JoinList [A]

	//join(Singleton(1), Singleton(2), 2)
	//l1 and l2 joining them (l1, l2, l1.size + l2.size)
	//joining lists like above is constant time because just making a new object
	//basically joined lists are singletons joined to eachother in joined lists.
	// Empty() makes empty Join, instead of Singleton(x), could do Join(Empty(), Singleton(x), 1)
	//Traversal go to the left first then go to the right for in order traversal of when the elements are 
	//added (how they are nested). Singletons are the only things that hold data, JoinLists are references  //singletons. Leverage the stack using recursion like in 187
	//reading the data from the left to right and call a method on the left. Read(n) { read(n, left) print( //n, value), read(n.right))}
	//Join(Join(Empty(), Singleton(1), 1), Singleton(2), 2) pathological makes a linear tree (unbalanced) 
	//No rebalancing, alist is to the left, blist is to the right
	//three cases when case matching JoinedLists(Empty, singleton, join) because sealed trait can reach the singletons to get to x
	//instead of returning null or special logic case, return option (either is some, whcih has data, or none, with no data) return each one 
	//Tuple with two lists for matching, check if both somes or nones Some(7) == Some(7) (is like Java .equals) could case match on Some(), Map over an Option, like sum (functions defined on lists are good for options)
	//JoinLists are Faster at appending then Lists, not removal though, faster at size if List does it recursively

	def max [A](lst: JoinList [A], compare: (A , A) => Boolean): Option [A] = lst match {
		case (Empty()) => None
		case (Singleton(x)) => x match { //messed up 
			case _ => Some(x)
		}
		case (Join(list1, list2, _)) => {
			val num1 = max(list1, compare)
			val num2 = max(list2, compare)
			(num1, num2) match{
				case (Some(num1), Some(num2)) => {
					if (compare(num1, num2)) Some(num2) 
					else Some(num1)
				}
				case (Some(num1), None) => Some(num1)
				case (None, Some(num2)) => Some(num2)
				case (None, None) => None
			} 
		}
	}

	def first [A](lst: JoinList [A]): Option [A] = lst match {
		case (Empty()) => None
		case (Singleton(x)) => x match { //messed up 
			case _ => Some(x)
		}
		case (Join(list1, list2, _)) => {
			if (list1.size != 0) first(list1)
			else first(list2)
		}
	}

	def rest [A](lst: JoinList [A]): Option [JoinList [A]] = lst match{
		case (Join(list1, list2, _)) => list1 match {
			case (Empty()) => Some(list2)
			case (Singleton(_)) => Some(list2)
			case (Join(lst1, lst2, _)) =>  
				val final1 = rest(lst1)
				final1 match{
					case Some(final1) => {
						val left = Join(final1, lst2, lst1.size-1 + lst2.size) //going too deep? 
						Some(Join(left, list2, left.size + list2.size))
					}
					case _ => Some(Join(lst2, list2, lst2.size + list2.size))

				}
		}
		case (Empty()) => None
		case (Singleton(_)) => None //Some(Empty()) //There is a problem here, cant return empty
	}
	
	def nth [A](lst: JoinList [A], n: Int): Option [A] = {
		if (lst.size -1 < n) None
		else if (n < 0) None
		else if (n != 0) {
			val list = rest(lst) //have to fix rest
			list match{
				case Some(list) => nth(list, n-1)
				case _ => None
			}
		} 
		else first(lst)
	}
	
	def map [A ,B](f: A => B , lst: JoinList [A]): JoinList [B] = lst match {
		case (Empty()) => Empty()
		case (Singleton(num)) => {
			Singleton(f(num)) 
		}
		case (Join(list1, list2, _)) => {
			Join(map(f, list1), map(f, list2), list1.size + list2.size) 
		}
	}
	
	def filter [A](pred: A => Boolean, lst: JoinList [A]): JoinList [A] = lst match {
		case (Empty()) => Empty()
		case (Singleton(num)) => {
			if (pred(num)) Singleton(num)
			else Empty() //how do you get rid of the empties? case match before you add them to the list? 
		}
		case (Join(list1, list2, _)) => {
				val l1 = filter(pred, list1)
				val l2 = filter(pred, list2)
				if (l1 != Empty() && l2 != Empty()) Join(l1, l2, l1.size + l2.size)
				else if (l1 != Empty()) l1
				else if (l2 != Empty()) l2
				else Empty()
		}
	}
}