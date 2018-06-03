import hw.sudoku._

object Solution extends SudokuLike{
	type T = Board
	def parse (str: String): Board = {
		val empty = (0.to(8).map(x => 0.to(8).toList.map(y => (x,y))).map(x => 1.to(9)))
		val chars = str.toCharArray.toList
	//	println(empty)
		// //Board(parseHelp(str, Map[(Int, Int), List[Int]]()))
		// val board = Board(Map[(Int, Int), List[Int]]())
		???
	}
	
	// def parseHelp (chars: List[Char], curr: Map[(Int, Int), List[Int]], row: Int, col: Int): Map[(Int, Int), List[Int]] = chars match {
	// 	case head :: tail => {
	// 		if(head == '.') {
	// 			val vert = (row, col).map(x => 1.to(9))
	// 		}
	// 	}
	// 	case Nil => {

	// 	}
 //      	Map[(Int, Int), List[Int]]()
	// }
	def parseHelp (chars: List[Char], curr: Map[(Int, Int), List[Int]], row: Int, col: Int): Boolean = chars match {
		case head :: tail => {
			if(head == '.') { //how the fuck to map tuples to list of ints??????????????????????????????????????????????????
				val vert = (row, col) //apparently cannot use tuples??
				// val newCurr = List(1, 2).map((x: List[Int]) => (x,  0.to(9)))
				// println(newCurr)		
				val empty = 0.to(8).map(x => 0.to(8).map(y => (x,y))).flatten.toList.map(x => (x, 1.to(9).toList)).map{case (x, y) => (x -> y)}.toMap
				println("empty board is " + empty)
				// val verts = List(row, col).map(x => 0.to(8).toList.map(y => (x, y))).toList.flatten
				// val mapped = verts.map(x => x)
				true
			}
			else true
		}
		case Nil => {
			true
		}
	}
	// You can use a Set instead of a List (or , any Iterable )
	
	def peers (row: Int, col: Int): List[(Int, Int)] = {
		val colVerts = List(col).map(x => 0.to(8).toList.map(y => (y,x))).flatten
		val rowVerts = List(row).map(x => 0.to(8).toList.map(y => (x,y))).flatten
		val rowCol = (colVerts ::: rowVerts).filter(x => !(x._1 == row && x._2 == col))
		//get the rest of the block
		if(col <= 2){ //top set
			if(row <= 2){ //left block
				val tL = 0.to(2).map(x => 0.to(2).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = tL ::: rowCol
				ret.toSet.toList
			}
			else if(row >= 3 && row <= 5){ //middle block
				val tM = 3.to(5).map(x => 0.to(2).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = (tM ::: rowCol).toSet.toList
				ret
			}
			else { //right block
				val tR = 6.to(8).map(x => 0.to(2).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = (tR ::: rowCol).toSet.toList
				ret
			}
		}
		else if (col >= 3 && col <= 5){ //middle blocks
			if(row <= 2){ //left block
				val mL = 0.to(2).map(x => 3.to(5).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = (mL ::: rowCol).toSet.toList
				ret
			}
			else if(row >= 3 && row <= 5){ //middle block
				val mM = 3.to(5).map(x => 3.to(5).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = (mM ::: rowCol).toSet.toList
				ret
			}
			else { //right block
				val mR = 6.to(8).map(x => 3.to(5).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = (mR ::: rowCol).toSet.toList
				ret
			}
		}
		else { //bottom blocks
			if(row <= 2){ //left block
				val bL = 0.to(2).map(x => 6.to(8).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = (bL ::: rowCol).toSet.toList
				ret
			}
			else if(row >= 3 && row <= 5){ //middle block
				val bM = 3.to(5).map(x => 6.to(8).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = (bM ::: rowCol).toSet.toList
				ret
			}
			else { //right block
				val bR = 6.to(8).map(x => 6.to(8).toList.map(y => (x, y))).toList.flatten.filter(x => !(x._1 == row && x._2 == col))
				val ret = (bR ::: rowCol).toSet.toList
				ret
			}
		}
	}
}

// Top - left corner is (0 ,0). Bottom - right corner is (8 ,8). Feel free to
// change the fields of this class .
class Board (val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

	def availableValuesAt (row: Int, col: Int): List[Int] = {
		// Assumes that a missing value means all values are available . Feel
		// free to change this .
		available.getOrElse((row, col), 1.to(9).toList)
	}

	def valueAt (row: Int, col: Int): Option[Int] = ???
	
	def isSolved (): Boolean = ???
	
	def isUnsolvable (): Boolean = ???
	
	def place (row: Int, col: Int, value: Int): Board = {
		require(availableValuesAt(row, col).contains(value))
		???
	}

	// You can return any Iterable (e.g., Stream )
	def nextStates (): List[Board] = {
		if (isUnsolvable()) {
			List()
		}
		else {
			???
		}
	}
	
	def solve(): Option[Board] = ???
}

/*
Eliminate as many invalid boards as possible
Constrain the other cells in the parsee function

Not going to be given an empty board.
Parsing is breaking up map and sending String of "...5..3.2.." to a Map of coordinates (update the values of peers to not include row columns placed)
Immediately remove invalid games from tree and travel back up (up and down saves run time)
Calue at specific coordinate (if it has one value, then return it) 
IsFinished() Every coordinate on the baord has one value in the List/Map
Place: Remove value from both row and column and block when you place it. 

Looked at every open spot in tictactoe, check each spot and place one of each possibile boards with the new numbers (eg 1, 3, 6) make three new boards with 
1, 3, 6 in that particular spot. (Look at all possibilities and place only one if no possibilities invalid and backtrack search) NextBoards is called a lot

see what happens when you add to spot (valid)
parse is fixed state List(one value) and look to see which moves are valid from that state (filter out cell you are adding (remove from peers that possibility)) Do both
at the same time 

mapped coordinates will have every possibility (0,0) -> (3, 4, 6, 7)? Use map, fold 
Go through algorithm step by step 

parsing each time doesnt make 
may be easier to start with board before parse
call isUnsolvable during parsing 

check piazza for whether or not board can be solved 
String list of characters and can map like a list 
iterate 81 times for mapping string  
*/