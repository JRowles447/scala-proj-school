
import hw.tictactoe._

object TicTacToeGame{

	case class Game(turn: Player, dim: Int, board: Map[(Int, Int), Player]) extends GameLike [Game] {
		def isFinished(): Boolean = { 
			if (board.size == dim*dim) true //checks to see if all spots on board filled
			else board match {
				case _ => {
					val listXs = board.filter(keyVal => keyVal match{case(k, v) => v==X})//Where the fuck did keyVal come from?
					val listOs = board.filter(keyVal => keyVal match{case(k, v) => v==O}) //Not filtering right					// listXs match{ //lsit
					if (isWinner(listXs)) true
					else if (isWinner(listOs)) true
					else false
				}
			}
		} //tree by tree list of all 
		
		/* Assume that isFinished is true */
		def getWinner(): Option[Player] = {
			val listXs = this.board.filter(keyVal => keyVal match{case(k, v) => v==X})//Where the fuck did keyVal come from?
			val listOs = this.board.filter(keyVal => keyVal match{case(k, v) => v==O})
			if (isWinner(listXs)) Some(X)
			else if (isWinner(listOs)) Some(O)
			else None
		}		

		def nextBoards(): List[Game] = {
			import Solution._
			val verts = 0.to(dim-1).map(x => 0.to(dim-1).toList.map(y => (x, y))).toList.flatten
			val mapped = verts.map(x => board.get(x))
			val zipped = verts.zip(mapped)
			val emptySpace = zipped.filter((x: ((Int, Int), Option[Player])) => x._2 == None).toMap.keys.toList //the list of empty spaces to be filled
			if (this.turn == X) fillBoards(emptySpace, List[Game](), O)
			else fillBoards(emptySpace, List[Game](), X)
		} 

		def fillBoards(available: List[(Int, Int)], ret: List[Game], nextTurn: Player): List[Game] = available match{
			case head :: tail =>{
				import Solution._
				val filled = ((head), this.turn) :: this.board.toList
				val mapped = filled.toMap
				val newGame = Solution.createGame(nextTurn, this.dim, mapped) :: ret
				fillBoards(tail, newGame, nextTurn)
			}
			case Nil => ret
		}
		
		
		def isWinner(positions: Map[(Int, Int), Player]): Boolean = { //helper method for getWinner Method			
			if (getWinnerRowRec(positions, 0, dim) != None) true
			else if (getWinnerColRec(positions, 0, dim) != None) true
			else if (getWinnerDiagonal(positions, dim) != None) true
			else if (getWinnerBDiag(positions, dim) != None) true
			else false
		}
	}

	def getWinnerRow(board: Map[(Int, Int), Player], rowNum: Int, dim: Int): Option[Player] = {
		val row = board.filter((x: ((Int, Int), Player)) => x._1._2 == rowNum).values.toList
		if (row.size != dim) None
		else{
			val rowSet = row.toSet
			if (rowSet.size == 1) Some(rowSet.head)
			else None	
		}
	} 

	def getWinnerCol(board: Map[(Int, Int), Player], colNum: Int, dim: Int): Option[Player] = {
		val col = board.filter((x: ((Int, Int), Player)) => x._1._1 == colNum).values.toList
		if (col.size != dim) None
		else{
			val colSet = col.toSet
			if (colSet.size == 1) Some(colSet.head)
			else None
		}	
	} 

	def getWinnerDiagonal(board: Map[(Int, Int), Player], dim: Int): Option[Player] = { 
		val diag = board.filter((x: ((Int, Int), Player)) => x._1._1 == x._1._2).values.toList
		if (diag.size != dim) None
		else{
			val diagSet = diag.toSet
			if(diagSet.size == 1) Some(diagSet.head)
			else None
		}
	}

	def getWinnerBDiag(board: Map[(Int, Int), Player], dim: Int): Option[Player] = { 
		val diag = board.filter((x: ((Int, Int), Player)) => (x._1._1 + x._1._2 == dim - 1)).values.toList
		if (diag.size != dim) None
		else{
			val diagSet = diag.toSet
			if(diagSet.size == 1) Some(diagSet.head)
			else None
		} 
	}

	def getWinnerRowRec(board: Map[(Int, Int), Player], row: Int, dim: Int): Option[Player] = {
		if (getWinnerRow(board, row, dim) != None) getWinnerRow(board, row, dim)
		else{
			if (row == (dim - 1)) None
			else getWinnerRowRec(board, row + 1, dim)
		}
	}	

	def getWinnerColRec(board: Map[(Int, Int), Player], col: Int, dim: Int): Option[Player] = {
		if(getWinnerCol(board, col, dim) != None) getWinnerCol(board, col, dim)
		else{
			if (col == (dim - 1)) None
			else getWinnerColRec(board, col + 1, dim)
		}
	}

	def printBoard(state: Game): String = {
			val verts = 0.to(state.dim-1).map(x => 0.to(state.dim-1).toList.map(y => (x, y))).toList.flatten
			val mapped = verts.map(x => state.board.get(x))
			val zipped = verts.zip(mapped)
			println(zipped)
			""
	}

	object Solution extends MinimaxLike{
		type T = Game // T is an "abstract type member" of MinimaxLike
		def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
			val game = new Game(turn, dim, board)
			game
		}

		def minimax(board : Game): Option[Player] = {
			if (board.turn == X){
				if (board.isFinished) board.getWinner 
				else {
						val newBoards = board.nextBoards
						val res = newBoards.map((x: Game) => minimax(x)) //returns the results of minimax on current state
						if (res.contains(Some(X))) Some(X)
						else if (res.contains(None)) None
						else Some(O)
				}
			}
			else {
				if (board.isFinished) board.getWinner
				else {
					val newBoards = board.nextBoards
					val res = newBoards.map((x: Game) => minimax(x))
					if (res.contains(Some(O))) Some(O)
					else if (res.contains(None)) None
					else Some(X)
				}
			}
		} 

	}
}
