import annotation.tailrec

class AI(private var player: Player, private var depth: Int) extends Solver {

  
  /**
   * Here we will have only 1 preferred move or the 
   * best move at the current state of board b
   */
  override def getMoves(b: Board): Array[Move] = {
    
    var s = new State(player, b, new Move(player.opponent, 0))
    AI.createGameTree(s, depth)
    if (s.children.length == 0) {   // No moves left, error
      System.out.println("Error: AI.getMoves state has no children!!")
      return Array[Move]()
    }
    AI.minimax(this, s)
    
    var maxValue = -10000000
    var column = -1
    
    System.out.println("KKK: "+ s.children.length)
    
    for (i <- 0 to s.children.length - 1) {    // Pick maximum value of children
      if (s.children(i).value > maxValue) {
        maxValue = s.children(i).value
        column = i
      }
    }     
    System.out.println("KKK: "+ column)
    Array(new Move(player, column))
  }
 
  class Node(var state: State, var node: Int)
 
  def minimax(s: State) {

    def mm(stack: List[Node]) {
      
      if (stack.length > 0) {
      var ss = stack.head.state  // Save some typing
      System.out.println(ss.board)
      System.out.println(ss.children.length)
      
      ss.children.length match {
       
                 // This is a leaf, so evaluate the board and move back to the parent
      
        case 0 => ss.value = evaluateBoard(ss.board)  // Leaf
                  System.out.println("Leaf:" + ss.value)
                  var ps = stack.tail                 // Parent   
                  if (this.player == ss.player) {     // Parent node is opponent, select minimum
                    if (ss.value < ps.head.state.value) {ps.head.state.value = ss.value}
                  } else { 
                    if (ss.value > ps.head.state.value) {ps.head.state.value = ss.value}
                  }                  
                  ps.head.node += 1                    // Move to next child
                  mm(ps)
                 
               // Children array size > next node, so still children to traverse
                 
         case ln if ln > stack.head.node => System.out.println(stack.head.node)
              mm(List(new Node(ss.children(stack.head.node), 0)):::stack) 
                 
  
         case _ => System.out.println("Kids done")
                     if (stack.length > 1) {
                     var ps = stack.tail                 // Parent   
                     if (this.player == ss.player) {     // Parent node is opponent, select minimum
                       if (ss.value < ps.head.state.value) {ps.head.state.value = ss.value}
                     } else { 
                       if (ss.value > ps.head.state.value) {ps.head.state.value = ss.value}
                     }                  
                     ps.head.node += 1                    // Move to next child
                     mm(ps)                               // Traversed all children
         }
      }
    } 
    }
    
    mm(List(new Node(s, 0)))        // Call helper function
      
  }

  /**
   * Evaluate the desirability of Board b for this player
   * Precondition: b is a leaf node of the game tree (because that is most
   * effective when looking several moves into the future).
   */
  def evaluateBoard(b: Board): Int = {
    val winner = b.hasConnectFour()
    var value = 0
    if (!winner.isDefined) {
      val locs = b.winLocations()
      for (loc <- locs; p <- loc) {
        value += (if (p == player) 1 else if (p != null) -1 else 0)
      }
    } else {
      var numEmpty = 0
      var r = 0
      while (r < Board.NUM_ROWS) {
        var c = 0
        while (c < Board.NUM_COLS) {
          if (b.getTile(r, c) == null) numEmpty += 1
          c = c + 1
        }
        r = r + 1
      }
      value = (if (winner.get == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
}

object AI {

  /**
   * Generate the game tree with root s of depth d.
   * The game tree's nodes are State objects that represent the state of a game
   * and whose children are all possible States that can result from the next move.
   * <p/>
   * NOTE: this method runs in exponential time with respect to d.
   * With d around 5 or 6, it is extremely slow and will start to take a very
   * long time to run.
   * <p/>
   * Note: If s has a winner (four in a row), it should be a leaf.
   */
 
	def createGameTree(s: State, d: Int) : Unit = {
			if (d != 0) {                       // Still within depth
        s.initializeChildren()
				var children = s.getChildren()
				if (!children.isEmpty) {          // Not a leaf, has children
					for (si <- children) {
              createGameTree(si, (d-1))
				}
			}
		}
  }

  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}
  