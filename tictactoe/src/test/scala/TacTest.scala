import TicTacToeGame._
import hw.tictactoe._
import Solution._

class TestSuite extends org.scalatest.FunSuite {

	val first = Solution.createGame(X, 3, Map((0, 0) -> X, (2, 2) -> O))
	val second = Solution.createGame(X, 3, Map((0, 0) -> X, (2, 2) -> O, (1, 1) -> X, (2, 0) -> X, (0, 1) -> O, (1, 0) -> X))
	val third = Solution.createGame(O, 3, Map((1, 0) -> O, (2,0) -> X, (1, 1) -> X, (0, 2) -> X, (2, 2) -> O))
	val fourth = Solution.createGame(O, 4, Map((2, 0) -> X, (3, 0) -> O, (1, 1) -> X, (2, 1) -> O, (0, 2) -> X, (0, 3) -> O, (1, 2) -> O, (2, 3) -> X))
	val empty = Solution.createGame(O, 3, Map())
	val secondOne = Solution.createGame(X, 3, Map((2, 2) -> O, (1, 1) -> X, (2, 0) -> X, (0, 1) -> O, (1, 0) -> X))
	val oneEmpty = Solution.createGame(O, 3, Map((0,0) -> O, (1,0) -> O, (0,1) -> O, (1,1) -> X, (2,1) -> X, (0,2) -> X, (1,2) -> O, (2,2) -> X))
	val full = Solution.createGame(X, 3, Map((0,0) -> X, (1,0) -> X, (0,1) -> X, (1,1) -> X, (2,1) -> X, (0,2) -> X, (1,2) -> X, (2,2) -> X))
	val fullO = Solution.createGame(X, 3, Map((0,0) -> X, (1,0) -> X, (0,1) -> X, (1,1) -> X, (2,1) -> X, (0,2) -> X, (1,2) -> X, (2,2) -> X))
	val fullDraw = Solution.createGame(O, 3, Map((0,0) -> O, (1,0) -> X, (2,0) -> O, (0,1) -> O, (1,1) -> X, (2,1) -> X, (0,2) -> X, (1,2) -> O, (2,2) -> X))
	val twoDraw = Solution.createGame(O, 3, Map((0,0) -> X, (2,0) -> O, (0,1) -> O, (1,1) -> X, (2,1) -> X, (0,2) -> X, (1,2) -> O))
	val oWins = Solution.createGame(O, 3, Map((2,0) -> O, (0,2) -> O, (0,0) -> O, (1,1) -> X))
	val emptyFours = Solution.createGame(X, 4, Map())
	val twoFours = Solution.createGame(X, 4, Map((0, 0) -> X, (1, 1)-> O))
	val alreadyDone = Solution.createGame(O, 4, Map((0,0) -> X, (1,0) -> O, (2,0) -> X, (3,0) -> O, (0,1) -> X, (1,1) -> O, (2,1) -> O, (3,1) -> X, (0,2) -> X, (1,2) -> O, (2,2) -> X, (3,2) -> O, (0,3) -> X, (1,3) -> O, (2,3) -> X, (3,3) -> O))
	val doneNotFull = Solution.createGame(X, 4, Map((0,0) -> X, (1,0) -> O, (2,0) -> X, (3,0) -> O, (0,1) -> X, (1,1) -> O, (2,1) -> O, (3,1) -> X, (0,2) -> X, (1,2) -> O, (2,2) -> X, (3,2) -> O, (0,3) -> X, (1,3) -> O))
	val xWins = Solution.createGame(O, 3, Map((0, 0) -> X, (2, 0) -> X, (1, 1) -> O, (2, 2) -> X))
	val xCorners = Solution.createGame(O, 3, Map((0, 0) -> X, (0, 2) -> X, (1, 1) -> O, (2, 0) -> X, (2,2) -> X))
	val oBorder = Solution.createGame(X, 4, Map((0,0) -> O, (1,0) -> O, (2,0) -> O, (0,1) -> O, (2,2) -> O, (0,3)-> O, (1,3) -> X, (3, 3) -> X))


	val row1 = Solution.createGame(X, 3, Map((0,0) -> O, (1,0) -> O, (2,0) -> O))
	val row2 = Solution.createGame(X, 3, Map((0,1) -> O, (1,1) -> O, (2,1) -> O))
	val row3 = Solution.createGame(X, 3, Map((0,2) -> O, (1,2) -> O, (2,2) -> O))
	val col1 = Solution.createGame(X, 3, Map((0,0) -> X, (0,1) -> X, (0,2) -> X))
	val col2 = Solution.createGame(X, 3, Map((1,0) -> X, (1,1) -> X, (1,2) -> X))
	val col3 = Solution.createGame(X, 3, Map((2,0) -> X, (2,1) -> X, (2,2) -> X))

	test ("isWinner test"){
		assert(first.isWinner(first.board) == false)
		assert(second.isWinner(second.board) == true)
		assert(third.isWinner(third.board) == true)
		assert(fourth.isWinner(fourth.board) == true)
		assert(empty.isWinner(empty.board) == false)
	}

	test ("isWinner test rows"){
		assert(row1.isWinner(row1.board) == true)
		assert(row2.isWinner(row1.board) == true)
		assert(row3.isWinner(row1.board) == true)
	}

	test ("isWinner test columns"){
		assert(col1.isWinner(col1.board) == true)
		assert(col2.isWinner(col1.board) == true)
		assert(col3.isWinner(col1.board) == true)
	}

	test("isFinished test"){
		assert(first.isFinished() == false)
		assert(second.isFinished() == true)
		assert(third.isFinished() == true)
		assert(first.isFinished() == false)
		assert(fourth.isFinished() == true)
		assert(empty.isFinished() == false)
	}

	test("Other isFinished Tests on rows"){
		assert(row1.isFinished == true)
		assert(row2.isFinished == true) 
		assert(row3.isFinished == true) //also failing
	}

	test("Columns is finished test"){
		assert(col1.isFinished == true) 
		assert(col2.isFinished == true) 
		assert(col3.isFinished == true) 
	}

	test("getWinner Test"){
		assert(first.getWinner == None)
		assert(second.getWinner == Some(X))
		assert(third.getWinner == Some(X))
		assert(fourth.getWinner == Some(O))
		assert(empty.getWinner == None)
	}

	test("row0 getWinner Test"){
		println("test row 0")
		assert(row1.getWinner == Some(O))
		// println("test row 1")
		// assert(row2.getWinner == Some(O))
		// println("test row 2")
		// assert(row3.getWinner == Some(O)) //Returns None
	}

	test("row 1 getWinner Test"){
		println("test row 1")
		assert(row2.getWinner == Some(O))
	}

	test("row 2 getWinner Test"){
		println("test row2")
		assert(row3.getWinner == Some(O)) //Returns None
	}

	test("columns getWinner Test"){
		assert(col1.getWinner == Some(X))
		assert(col2.getWinner == Some(X))
		assert(col3.getWinner == Some(X)) //Returns None shouldnt
	}

	test("nextBoards Test"){
		assert(first.nextBoards.length == (first.dim*first.dim - first.board.size))
		assert(second.nextBoards.length == (second.dim*second.dim - second.board.size))
		assert(third.nextBoards.length == (third.dim*third.dim - third.board.size))
		assert(fourth.nextBoards.length == (fourth.dim*fourth.dim - fourth.board.size))
		assert(empty.nextBoards.length == (empty.dim*empty.dim - empty.board.size))

		first.nextBoards
	}

	test("miniMax Test"){
		println("Test for Fourth")
		assert(minimax(fourth) == Some(O))
		println("Test for empty")
		assert(minimax(empty) == None)
		assert(minimax(fullO) == Some(X))
		println("Test for empty done")
		assert(minimax(secondOne) == Some(X))
		println("Test for secondOne done")
		assert(minimax(full) == Some(X))
		assert(minimax(fullO) == Some(X))
		assert(minimax(fullDraw) == None)
		assert(minimax(twoDraw) == None)
		assert(minimax(oWins) == Some(O))
		assert(minimax(oneEmpty) == Some(O))
		// println("twoFours test has begun!")
		// assert(minimax(twoFours) == None)
		// println("twoFours test has passed")
		// println("emptyFours test has begun")
		// assert(minimax(emptyFours) == None)
		// println("emptyFours test has passed!")
	}

	test("minimax Test 2"){
		assert(minimax(xCorners) == Some(X))
		assert(minimax(oBorder) == Some(O))
		assert(minimax(xWins) == Some(X))
		assert(minimax(alreadyDone) == Some(X))
		assert(minimax(doneNotFull) == Some(X))

	}

}