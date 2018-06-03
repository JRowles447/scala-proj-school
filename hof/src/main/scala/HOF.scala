object HOF {

	//Exercise 1
	def map2 [A, B, C](f : (A, B) => C, lst1 : List [A], lst2 : List [B]) : List [C] = (lst1, lst2) match {
		case (Nil, Nil) => Nil 
		case (h :: t, head :: tail) => f(h, head) :: map2[A, B, C](f, t, tail)
		case _ => Nil
	}
	
	//Exercise 2
	def zip [A ,B](lst1 : List [A], lst2 : List [B]): List [(A, B)] = (lst1, lst2) match {
		case (Nil, Nil) => Nil
		case (h :: t, head :: tail) => (h, head) :: zip(t, tail) 
		case (List(_), Nil) => ???
		case (Nil, List(_)) => ???
	}

	//Exercise 3	the list is inside of a list so could have List((1, 2), (5, 6), (5, 6, 7)) use map2
	def flatten [A](lst : List [List [A]]): List [A] = lst match {
		case head :: Nil => head
		case head :: tail :: Nil => append(head, tail)
		case head :: tail :: others => {
			append(append(head, tail), flatten(others))
		}
		case Nil => Nil
	}

	//Exercise 4	
	def flatten3 [A](lst : List [List [List [A]]]): List [A] = lst match{
		case head :: tail => append(flatten(head), flatten3(tail))
		case Nil => Nil 
	}

	//Exercise 5	
	def buildList [A](length : Int, f: Int => A): List [A] = {
		if (length == 0) List[A]()
		else func(length, 0, f)
	}
	
	def func[A](leng : Int, x : Int, f : Int => A): List[A] = {
		if (x >= leng) List()
		else {
			f(x) :: func(leng, x +1, f)
		}
	}

	//Exercise 6 
	def mapList [A , B](lst : List [A], f: A => List [B]): List [B] = lst match {
		case Nil => Nil 
		case head :: tail => append(f(head), mapList(tail, f))  
	}

	// helper method 
	def append [A](lst1 : List[A], lst2 : List[A]) : List[(A)] = (lst1, lst2) match {
		case (h :: t, head :: tail) => h :: append(t, lst2) 
		case (h :: t, Nil) => h :: append(t, lst2)
		case (Nil, h :: Nil) => List(h)
		case (Nil, h :: t) => h :: append (Nil, t)
		case (Nil, Nil) => List()
		case (_) => println("FAILURE"); List() 
	}
	
	//Exercise 7
	def partition [A](f : A => Boolean, lst : List [A]): (List [A], List [A]) = lst match {
		case h :: t => partHelp(f, lst, List(), List())
		case Nil => (List(), List())
	}

	//helper method for partition
	def partHelp [A](f : A => Boolean, lst1 : List[A], out1 : List[A], out2 : List[A]) : (List[A], List[A]) = lst1 match {
		case h :: t => {
			if (f(h)) partHelp(f, t, append(out1, List(h)), out2) 
			else partHelp(f, t, out1, append(out2, List(h))) 
		}
		case Nil => (out1, out2)

	}

	//Exercise 8
	def merge [A](lessThan : (A, A) => Boolean , alist1 : List [A], alist2 : List [A]): List [A] = (alist1, alist2) match {
		case (h :: t, head :: tail) => {
			if(lessThan(h, head)) head :: merge (lessThan, alist1, tail)
			else h :: merge (lessThan, t, alist2)
		}
		case (h :: t, Nil) => h :: merge(lessThan, t, Nil)
		case (Nil, h :: t) => h :: merge(lessThan, Nil, t)
		case _ => List()
	}
	
	//Exercise 9
	def sort [A](lessThan : (A, A) => Boolean, alist : List [A]): List [A] = alist match {
		case h :: t :: Nil => merge(lessThan, List(h), List(t)) //sort the end and work back
		case h :: Nil => List(h)
		case h :: t => {
			sortHelper(lessThan, List(h), t)			
		}
		case Nil => List()
		case _ => List()
	}

	//helper method for sort 
	def sortHelper[A](lessThan : (A, A) => Boolean, sorted : List[A], unsorted : List[A]): List[A] = (sorted, unsorted) match {
		case (_, h :: t) => {
			val newList = merge (lessThan, sorted, List(h))
			sortHelper(lessThan, newList, t)
		}
		case (_, Nil) => sorted
	}

}