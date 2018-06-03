object Lists {
	val oddNumbers = 1 :: 3 :: 5 :: Nil

	//Exercise 2.1
	def sumDouble(alist: List[Int]): Int = alist match {
		case Nil => 0
		case n :: tail => n * 2 + sumDouble(tail)
	}

	// //Exercise 2.2 
	def removeZeros(alist: List[Int]): List[Int] = alist match {
		case Nil => List()
		case a :: b => { 
			if (a == 0) removeZeros(b) 
			else a :: removeZeros(b)
		}
	}

	//Exercise 2.3
	def countEvens(alist: List[Int]): Int = alist match{
		case Nil => 0
		case head :: tail => 
			if (head % 2 == 0) 1 + countEvens(tail)
			else countEvens(tail)
	}

	//Exercise 2.4 
	def removeAlternating(alist: List[String]): List[String] = alist match{
		case Nil => List[String]()
		case a :: Nil => List[String](a)
		case a :: b :: Nil => List[String](a)
		case a :: b :: c => a :: removeAlternating(c)
		case List(_) => List[String]()
	}

	//Exercise 2.5 
	def isAscending(alist: List[Int]): Boolean = alist match{ //some problem with overloaded method value >
		case Nil => true
		case a :: Nil => true
		case a :: b => {
			if (a > b.head) false
			else isAscending(b) 
		}
		case List(_) => false
	}

	//Exercise 2.6 
	def addSub(alist: List[Int]): Int = alist match{
		case Nil => 0
		case head :: Nil => head
		case head :: tail :: others => head - tail + addSub(others)
	}

	//Exercise 2.7
	def alternate(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
		case (Nil, Nil) => List()
		case (head :: tail, h :: t) => head :: h :: alternate(tail, t)
		case (List(_), Nil) => List[Int]()
		case (Nil, List(_)) => List[Int]()
	}


	//Exerise 2.8 
	//Can we assume that the inputs will always be Ints? If not how should I change the case?
	def fromTo(from: Int, to: Int): List[Int] = (from, to) match{
		case _ => {
			if(from < to) from :: fromTo(from + 1, to)
			else List()
		}
	}

	//Exercise 2.9
	def insertOrdered(n: Int, lst: List[Int]): List[Int] = lst match{
		case Nil => List[Int](n)
		case head :: Nil => {
			if (n < head) List(n, head)
			else List(head, n)
		}
		case head :: tail :: Nil => {
			if (n <= head) List (n, head, tail)
			else if (n >= head && n <= tail) List[Int](head, n , tail)
			else List[Int](head, tail, n)
		}
		case head :: tail :: others => { 
			if (n < head) n :: lst
			else if (n >= head && n <= tail) head :: n :: tail :: others
			else head :: insertOrdered(n, tail :: others)
		}
	}

	//Exercies 2.10
	def sort(lst: List[Int]): List[Int] = lst match{
		case Nil => List[Int]()
		case num :: Nil => List[Int](num)
		case num :: others => insertOrdered(num, sort(others)) 
	}
}