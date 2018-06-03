import hw.sudoku._
import Solution._

class TrivialTestSuite extends org.scalatest.FunSuite {
	
	val fromCS121_1 = " 85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58 "

	test ("The solution object must be defined") {
		val obj: hw.sudoku.SudokuLike = Solution
	}

	test ("Peers test "){
		assert(peers(1, 1).length == 20)
		assert(peers(0, 0).length == 20)
		assert(peers(3, 2).length == 20)
		assert(peers(8, 1).length == 20)
		assert(peers(0, 5).length == 20)
		assert(peers(5, 3).length == 20)
		assert(peers(7, 5).length == 20)
		assert(peers(2, 6).length == 20)
		assert(peers(8, 7).length == 20)
		assert(peers(5, 6).length == 20)
		assert(peers(3, 3).length == 20)
		assert(peers(8, 8).length == 20)
	}

	test ("Parse Helper test"){
		parseHelp(List('.', '.', '.'), Map[(Int, Int), List[Int]](), 1, 1)
	}
}
