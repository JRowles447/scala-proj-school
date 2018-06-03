import Wrangling._
import edu.umass.cs.CSV

class TestSuite extends org . scalatest . FunSuite {

	val listCDC = CSV.fromFile("cdc-life-expectancy.csv")
	val listSSA = CSV.fromFile("ssa-births.csv")
	val short = CSV.fromFile("shortened-births.csv")
	val lifex = CSV.fromFile("cdc-life-expectancy.csv")
	
	val list80 = List(List("1880", "Mary", "F", "7065"), List("1880", "Charley", "M", "305"))
	val list81 = List(List("1881", "Mary", "F", "6919"), List("1881", "Francis", "M", "259"))
	val list82 = List(List("1882", "Della", "F", "447"), List("1882", "Elmer", "M", "441"), List("1882", "Marian", "M", "5"))
	val list83 = List(List("1883", "Bettye", "F", "5"), List("1883", "Whit", "M", "6"))
	val list84 = List(List("1884", "Ammie", "F", "7"), List("1884", "Lindsey", "M", "9"))
	val list85 = List(List("1885", "Anastasia", "F", "15"), List("1885", "Loyd", "M", "24"))
	val list13 = List(List("2013", "Deborah", "F", "329"), List("2013", "Rontavious", "M", "5"))
	val list14 = List(List("2014", "Alani", "F", "470"), List("2014", "Jianna", "F", "132"), List("2014", "Luca", "F", "67"))
	val mostPop = list80 ::: list81
	val sample = list80 ::: list81 ::: list82 ::: list83 ::: list84 ::: list85
	val testPop = List(List("1880", "Harry", "F", "5"), List("1880", "Charley", "M", "9"), List("1880", "Harry", "F", "5"), List("1880", "Sara", "F", "5"), List("1880", "Harry", "F", "5"), List("1880", "Charley", "F", "4"))
	val neut = List(List("2014", "Alice", "F", "470"), List("2014", "Alice", "M", "132"), List("2014", "Luca", "F", "67"))
	val listNew = list85 ::: List(List("1885", "Anastasia", "M", "15"), List("1885", "Loyd", "F", "24"))
	val neutLong = neut ::: listNew

	//Exercise 1 
	test("yearIs test"){
		//println(short)
		assert(yearIs(short, 1880) == list80)
		assert(yearIs(short, 1881) == list81)
		assert(yearIs(short, 1882) == list82)
		assert(yearIs(short, 1883) == list83)
		assert(yearIs(short, 1884) == list84)
		assert(yearIs(short, 1885) == list85)
		assert(yearIs(short, 2013) == list13)
		assert(yearIs(short, 2014) == list14)
	}

	//Exercise 2 
	test("yearGT test"){
		assert(yearGT(short, 2013) == list14)
		assert(yearGT(short, 2012) == list13 ::: list14)
		assert(yearGT(short, 2014) == List())	
	}

	//Exercise 3
	test("yearLT test"){
		assert(yearLT(short, 1881) == list80)
		assert(yearLT(short, 1882) == list80 ::: list81)
		assert(yearLT(short, 1886) == list80 ::: list81 ::: list82 ::: list83 ::: list84 ::: list85)
		assert(yearLT(short, 1) == List())
	}

	//Exercise 4
	test("onlyName test"){
		assert(onlyName(short, "Mary") == List(List("1880", "Mary", "F", "7065"), List("1881", "Mary", "F", "6919")))
		assert(onlyName(short, "Wowie") == List())
		assert(onlyName(short, "Henry") == List(List("2006", "Henry", "M", "4664")))
	}

	//Exercise 5
	test("mostPopular test"){
		assert(mostPopular(list80) == ("Mary", 7065))
		assert(mostPopular(mostPop) == ("Mary", 13984))
		assert(mostPopular(short) == ("Scott", 26050))
		assert(mostPopular(testPop) == ("Harry", 15))
	}

	//Exercise 6
	test("count test"){
		assert(count(list84) == 16)
		assert(count(list80) == 7370)
		assert(count(list83) == 11)
		assert(count(list85) == 39)
		assert(count(short) == 309151)
	}

	//Exercise 7
	test("girlsAndBoys test"){
		assert(countGirlsAndBoys(list80) == (7065, 305))
		assert(countGirlsAndBoys(list81) == (6919, 259))
		assert(countGirlsAndBoys(list82) == (447, 446))
		assert(countGirlsAndBoys(list83) == (5, 6))
		assert(countGirlsAndBoys(list80 ::: list81) == (13984, 564))
		assert(countGirlsAndBoys(sample) == (14458, 1049))
		assert(countGirlsAndBoys(List()) == (0, 0)) //what should it return if it is empty?
	}

	//Exercise 8
	test("genderNeutralNames test"){
		assert(genderNeutralNames(neut) == Set("Alice"))
		assert(genderNeutralNames(neutLong) == Set("Alice", "Anastasia", "Loyd"))
		assert(genderNeutralNames(list80) == Set())
	}
	//Exercise 9 
	test("expectedAlive test"){
		assert(expectedAlive("M", 1992, 2035) == true)
		assert(expectedAlive("M", 1900, 2035) == false)
		assert(expectedAlive("M", 2011, 2035) == false)
		assert(expectedAlive("F", 1930, 1992) == true)
		assert(expectedAlive("F", 1930, 1993) == false)
		assert(expectedAlive("M", 1950, 2300) == false)
		assert(expectedAlive("F", 2010, 2086) == true)
		assert(expectedAlive("F", 2010, 2087) == true)
		assert(expectedAlive("M", 2005, 2005) == true)
	}

	val list1932 = List(List("1932", "Earl", "F", "32"), List("1932", "Lothar", "M", "8"), List("1932", "Zonie", "F", "5"))
	val list1931 = List(List("1931", "Mitsuko", "F", "13"), List("1931", "Loyce", "M", "25"), List("1931", "Neomia", "F", "29"))
	val list1930 =  List(List("1930", "Vertie", "F", "41"), List("1930", "Earlie", "M", "52"), List("1930", "Elizabeth", "F", "6"))
	val list12 =  List(List("2012", "Costa", "M", "6"), List("2012", "Octavius", "M", "38"))
	val begin = list1930 ::: list1931 ::: list1932
	//Exercise 10
	test("estimatePopulation test"){
		assert(estimatePopulation(begin, 1931) == 99)
		assert(estimatePopulation(begin, 1930) == 0)
		assert(estimatePopulation(begin, 1932) == 166)
		assert(estimatePopulation(list1930, 1990) == 47)
		assert(estimatePopulation(list1930, 1993) == 0)
		assert(estimatePopulation(begin, 1993) == 79)
	}
}
