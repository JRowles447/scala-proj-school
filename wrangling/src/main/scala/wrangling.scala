
object Wrangling {
	import edu.umass.cs.CSV
	
	/* 
	How do I actually access the information that is in the lists, do I have to flatten them into 
	one list and search for the data? or do I have to search through the lists and check the first 
	element and match it to what I am looking for??? TAs will probably discuss Wednesday, if not 
	piazza post or research 
	*/
	val lifex = CSV.fromFile("cdc-life-expectancy.csv")

	//Exercise 1
	def yearIs(data: List[List[String]], n: Int): List[List[String]] = {
		//need to search the list of lists for a list whose head is a given year, 
		//must parse year into int to compare it to n CAN USE FILTER? check on the first element
		//Can use List x, x(n) to access the nth element implicit function call
		data.filter((x: List[String]) => x(0).toInt == n)
	}

	//Exercise 2
	def yearGT(data: List[List[String]], bound: Int): List[List[String]] = {
		data.filter((x: List[String]) => x(0).toInt > bound)
	}

	//Exercise 3
	def yearLT(data: List[List[String]], bound: Int): List[List[String]] = {
		data.filter((x: List[String]) => x(0).toInt < bound)
	}

	//Exercise 4
	def onlyName(data: List[List[String]], name: String): List[List[String]] = {
		data.filter((x: List[String]) => x(1) == name)   //could have done filter(x => x(1) == name) how does it know x is of tpe List[Strings]? type inference
	}

	//Exercise 5  
	def mostPopular(data: List[List[String]]): (String, Int) = {
		val map = data.groupBy((x: List[String]) => x(1))
		val summed = map.transform((_, (x: List[List[String]])) => count(x))
		//val ordered = summed.toSeq.sortBy(_._2)
		// println(ordered)
		summed.maxBy(_._2)
		//ordered.maxBy(_._2)
	}

	// //Exercise 6 print num children born in list of lists of births, keep running total 
	def count(data: List[List[String]]): Int = {
		if (!data.isEmpty){
			val flat = data.transpose
			val nums = flat(3).map((x: String) => x.toInt)
			nums.sum
		}
		else 0
	}

	//Exercise 7
	def countGirlsAndBoys(data: List[List[String]]): (Int, Int) = { 
		if (!data.isEmpty){
			val flat = data.transpose
			val zipped = flat(2).zip(flat(3))
			val ret = girlsHelper(zipped, 0, 0)
			val girls = ret._2
			val boys = ret._3
			(girls.toInt, boys.toInt)
			}
		else (0, 0)
	}

	def girlsHelper(list: List[(String, String)], girls: Int, boys: Int): (List[(String, String)], Int, Int) = list match {
		case head :: tail => {
			head match{
				case (h, t) => {
					if (h == "M") girlsHelper(tail, girls, boys + t.toInt)
					else girlsHelper(tail, girls + t.toInt, boys)
				}
			}
		}
		case Nil => {
			(List(("", ""), ("", "")), girls, boys)
		}

	}

	//Exercise 8
	def genderNeutralNames(data: List[List[String]]): Set[String] = {
		val males = data.filter((x: List[String]) => x(2) == "M")
		val females = data.filter((x: List[String]) => x(2) == "F")
		val mSet = males.transpose
		val namesM = mSet(1)
		val fSet = females.transpose
		val namesF = fSet(1)
		 
		println(namesM)
		println(namesF)

		namesM.toSet.intersect(namesF.toSet) 
		//Set("", "")
	}

	//Exercise 9
	def expectedAlive(gender: String, birthYear: Int, currentYear: Int): Boolean = {
		if (birthYear < 1930 | birthYear > 2010) false //outside the data on the CDC 
		else{ 
			val year = lifex.filter((x: List[String]) => x(0).toInt == birthYear).flatten
			if(gender == "M"){
				val lifeExp = year(1).toInt
				val howOld = currentYear - birthYear
				if ((howOld >= 0 ) && (howOld <= lifeExp)) true
				else false
			} 
	 		else{
	 			val lifeExp = year(2).toInt
	 			val howOld = currentYear - birthYear
				if ((howOld >= 0 ) && (howOld <= lifeExp)) true
				else false
	 		} 
	 	}
	}

	//Exercise 10 LT produces a list strictly less than the currentYear meaning that if the child was 
	//born in year, it was not counted. If Arjun changes his mind I will have to fix this 
	def estimatePopulation(data: List[List[String]], year: Int): Int = {
		val less = yearLT(data, year)
		val alive = less.filter((x: List[String]) => expectedAlive(x(2), x(0).toInt, year) == true)
		count(alive)
	}
}