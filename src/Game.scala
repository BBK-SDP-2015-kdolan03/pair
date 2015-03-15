 /* -------------------------------------------------------------- */

        /* ABOUT TESTING.
         * Testing is difficult using a JUnit testing class because it is difficult to
         * get at many of the fields and methods from that class. So you may want to
         * put some testing methods in this class.
         * 
         * In testing various methods, you may want to use boards with certain
         * layouts of your choosing. For example, a board with column 0 all filled.
         * To do this, you can
         * 
         *     1. Create a board b
         *     2. Call makeMove several times to put pieces where you want them.
         *     3. Play the game.
         *     
         * For example, you can write the following to set up an initial board and
         * then play the game
         * 
         *      Board b= new Board();
         *      b.makeMove(new Move(Player.RED, 4));
         *      b.makeMove(new Move(Player.YELLOW, 3));
         *      b.makeMove(new Move(Player.RED, 5));
         *      Game game= new Game(p1, p2, b, false);
         * 
         * This code places a red piece in column 4, a yellow piece in column 3,
         * and a red piece in column 5. Then it runs the game.
         * 
         * We give you also procedure fillColumn at the end of this file to help
         * out in initializing a board. Study it. Note that it is static.
         * 
         * Suppose you want to test a method that your wrote, like Board.getPossibleMoves.
         * Thus, you want to do the following.
         * 
         *     1. Create a board b
         *     2. Call makeMove several times to put pieces where you want them.
         *     3. Call the method you want to test.
         *     4. Check the result, if any.
         *     
         * You can check the result by using println statements to print out things and
         * looking at the output. You are testing by eyeballing the output. This is OK as
         * long as you are careful. Here is how you could print out the results of a
         * call to getPossibleMoves:
         * 
         *      Board b= new Board();
         *      fillColumn(b, Player.RED, 0);  // fill column 0
         *      Move[] moves= b.getPossibleMoves(Player.RED);
         *      for (Move m : moves) {
         *           System.out.println(m);
         *
         * If you are having real trouble, the above may not help. Here is what you
         * can do to test the very basics of Board.getPossibleMoves:
         * 
         *      Board b= new Board();
         *      fillColumn(b, Player.RED, 0);  // fill column 0
         *      Move[] moves= b.getPossibleMoves(Board.Player.RED);
         *      if (moves.length != Board.NUM_COLS-1) {
         *          System.out.println("Error in getPossibleMoves with 1 col filled. array size is wrong: " +  moves.length);
         *      }
         *      if (moves[0].getColumn() != 1) {
         *          System.out.println(s + "First col is filled, second isn't but moves[0] is " + moves[0]);
         *      } 
         *      
         * We suggest you write a static method to test getPossibleMoves
         * (and perhaps other methods for other tests).
         * Make it a method so that you can call it or not from method main, depending on
         * your needs. It doesn't have to test ALL possible cases, but it has to check
         * enough that so you are positive the method works.
         * 
         * 
        }
         * 
         * */


class Game(private var activePlayer: Solver, private var player2: Solver) {

  private var board: Board = Board()

  private var gui: GUI = _

  private var winner: Option[Player] = _

  def this(p1: Solver,
           p2: Solver,
           b: Board,
           p: Boolean) {
    this(p1, p2)
    board = b
    activePlayer = (if (p) p1 else p2)
  }

  def setGUI(gui: GUI) {
    this.gui = gui
  }

  def columnClicked(col: Int) {
    if (activePlayer.isInstanceOf[Human]) {
      activePlayer.asInstanceOf[Human].columnClicked(col)
    }
  }

  def runGame() {
    while (!isGameOver) {
      var moveIsSafe = false
      var nextMove: Move = null
      while (!moveIsSafe) {
        val bestMoves = activePlayer.getMoves(board)
        if (bestMoves.length == 0) {
          gui.setMsg("Game cannot continue until a Move is produced.")
          //continue
        } else {
          nextMove = bestMoves(0)
        }
        if (board.getTile(0, nextMove.column) == null) {
          moveIsSafe = true
        } else {
          gui.setMsg("Illegal Move: Cannot place disc in full column. Try again.")
        }
      }
      board.makeMove(nextMove)
      if (gui == null) {
        println(nextMove)
        println(board)
      } else {
        gui.updateGUI(board, nextMove)
      }
      val temp = activePlayer
      activePlayer = player2
      player2 = temp
      try {
        Thread.sleep(Game.SLEEP_INTERVAL)
      } catch {
        case e: InterruptedException => e.printStackTrace()
      }
    }
    if (gui == null) {
      if (winner.isDefined) {
        println(winner + " won the game!!!")
      } else {
        println("Tie game!")
      }
    } else {
      gui.notifyGameOver(winner.get)
    }
  }

  def isGameOver(): Boolean = {
    winner = board.hasConnectFour()

    if (winner.isDefined) return true
    var r = 0
    while (r < Board.NUM_ROWS) {
      var c = 0
      while (c < Board.NUM_COLS) {
        if (board.getTile(r, c) == null) return false
        c = c + 1
      }
      r = r + 1
    }
    true
  }
}

object Game extends App {

  val p1 = Dummy(RED)
  val p2 = Dummy(YELLOW)
  val game = Game(p1, p2)
  private val SLEEP_INTERVAL = 10
  game.runGame()

  def apply(p1: Solver, p2: Solver) =
    new Game(p1, p2)
}

