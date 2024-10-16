package minesweeper

import org.scalatest.funsuite.AnyFunSuite
import minesweeper.game.*

class TextUI extends AnyFunSuite {
  var new_state = GameState(Solution(solution_board, 2),
                            PlayerPool(List(player_0, player_1), 0),
                            GameStatus.Continue)

  val test_pos = Coordinate(3, 3)

  val player_0 = PlayerState(Player(0), new_player_board)
  val player_1 = PlayerState(Player(1), new_player_board)
  
  // [E][2][x]
  // [E][2][x]
  // [E][1][1]
  val solution_board = Board[SolutionTile](3, 3, Map(
      (Coordinate(1,1) -> SolutionTile.Empty), (Coordinate(1,2) -> SolutionTile.Hint(2)), (Coordinate(1,3) -> SolutionTile.Mine), 
      (Coordinate(2,1) -> SolutionTile.Empty), (Coordinate(2,2) -> SolutionTile.Hint(2)), (Coordinate(2,3) -> SolutionTile.Mine), 
      (Coordinate(3,1) -> SolutionTile.Empty), (Coordinate(3,2) -> SolutionTile.Hint(1)), (Coordinate(3,3) -> SolutionTile.Hint(1))
    ))

  val new_player_board = Board[PlayerTile](3, 3, Map(
    (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Hidden), (Coordinate(1,3) -> PlayerTile.Hidden), 
    (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Hidden), 
    (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Hidden), (Coordinate(3,3) -> PlayerTile.Hidden)
  ))


  /*
  test("player action") {
    println(s"new_state board: ${new_state.player_pool.current().board}")

    val next_state = play(new_state, PlayerAction(Action.Reveal, test_pos))
    val current_player = next_state.player_pool.current()
    
    assert(current_player.board.tile_map(test_pos) != PlayerTile.Hidden)
    assert(current_player.board.tile_map(test_pos) == PlayerTile.Revealed(SolutionTile.Hint(1)))
  }

  test("play-reveal-test_pos") {
    val next_game_state = play(new_state, PlayerAction(Action.Reveal, test_pos))
    println(s"current_player is: ${next_game_state.player_pool.current().id.n}")
    println(s"after play: ${next_game_state.player_pool.current().board.tile_map(test_pos)}")
    
    assert(next_game_state.player_pool.current().board.tile_map(test_pos) == PlayerTile.Revealed(SolutionTile.Hint(1)))
  }

  test("all_mines_flagged-true") {
    // [ ][ ][F]
    // [ ][ ][F]
    // [ ][ ][ ]
    val test_board = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Hidden), (Coordinate(1,3) -> PlayerTile.Flagged(Player(PlayerID(0), new_player_board, PlayerColor(PrinterEffects.Red)))), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(Player(PlayerID(1), new_player_board, PlayerColor(PrinterEffects.Blue)))), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Hidden), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    assert(all_mines_flagged(test_board, solution_board) == true)
  }

  test("all_mines_flagged-false") {
    // [][][ ]
    // [][][F]
    // [][][ ]
    val test_board = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Hidden), (Coordinate(1,3) -> PlayerTile.Hidden), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(Player(PlayerID(1), new_player_board, PlayerColor(PrinterEffects.Blue)))), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Hidden), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    assert(all_mines_flagged(test_board, solution_board) == false)
  }

  test("num_flagged_mines_per_player_2_0") {
    // [   ][     ][F.0]
    // [   ][     ][F.0]
    // [   ][1|F.1][   ]
    val test_board_0 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Hidden), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.RNF(SolutionTile.Hint(1), player_1)), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))

    // [   ][ 2 ][F.0]
    // [   ][   ][F.0]
    // [   ][F.1][   ]
    val test_board_1 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Revealed(SolutionTile.Hint(2))), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Flagged(player_1)), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    
    val test_player_0_1 = Player(PlayerID(0), test_board_0, PlayerColor(PrinterEffects.Red))
    val test_player_1_1 = Player(PlayerID(1), test_board_1, PlayerColor(PrinterEffects.Blue))
    
    assert(num_flagged_mines_per_player(PlayerPool(List(test_player_0_1, test_player_1_1), 1), solution_board) == List(2, 0))
  }

  test("num_flagged_mines_per_player_1_1") {
    // [   ][     ][F.0]
    // [   ][     ][F.1]
    // [   ][1|F.1][   ]
    val test_board_0 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Hidden), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_1)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.RNF(SolutionTile.Hint(1), player_1)), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))

    // [   ][ 2 ][F.0]
    // [   ][   ][F.1]
    // [   ][F.1][   ]
    val test_board_1 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Revealed(SolutionTile.Hint(2))), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_1)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Flagged(player_1)), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    
    val test_player_0_1 = Player(PlayerID(0), test_board_0, PlayerColor(PrinterEffects.Red))
    val test_player_1_1 = Player(PlayerID(1), test_board_1, PlayerColor(PrinterEffects.Blue))
    
    assert(num_flagged_mines_per_player(PlayerPool(List(test_player_0_1, test_player_1_1), 1), solution_board) == List(1, 1))
  }

  test("has_won_1-all-mines-flagged-by-player0") {
    // [][][F.0]
    // [][][F.0]
    // [][][   ]
    val test_board_0 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Hidden), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Hidden), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    

    // [][2][F.0]
    // [][ ][F.0]
    // [][ ][   ]
    val test_board_1 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Flagged(player_1)), (Coordinate(1,2) -> PlayerTile.Revealed(SolutionTile.Hint(2))), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Hidden), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    
    val test_player_0_1 = Player(PlayerID(0), test_board_0, PlayerColor(PrinterEffects.Red))
    val test_player_1_1 = Player(PlayerID(1), test_board_1, PlayerColor(PrinterEffects.Blue))
    
    // assert(all_mines_flagged(test_player_0_1.board, solution_board) == true)
    assert(has_won_1(PlayerPool(List(test_player_0_1, test_player_1_1), 1), 2, solution_board) == GameStatus.Win(Set(test_player_0_1)))
  }

  test("has_won_1-tie") {
    // [][][F.0]
    // [][][F.1]
    // [][][   ]
    val test_board_0 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Hidden), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_1)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Hidden), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    

    // [][2][F.0]
    // [][ ][F.1]
    // [][ ][   ]
    val test_board_1 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Flagged(player_1)), (Coordinate(1,2) -> PlayerTile.Revealed(SolutionTile.Hint(2))), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_1)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Hidden), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    
    val test_player_0_1 = Player(PlayerID(0), test_board_0, PlayerColor(PrinterEffects.Red))
    val test_player_1_1 = Player(PlayerID(1), test_board_1, PlayerColor(PrinterEffects.Blue))
    
    // assert(all_mines_flagged(test_player_0_1.board, solution_board) == true)
    assert(has_won_1(PlayerPool(List(test_player_0_1, test_player_1_1), 1), 2, solution_board) == GameStatus.Win(Set(test_player_0_1, test_player_1_1)))
  }

  test("num_flagged_non_mines_per_player") {
    // [   ][     ][F.0]
    // [   ][     ][F.1]
    // [   ][1|F.1][   ]
    val test_board_0 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Hidden), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_1)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.RNF(SolutionTile.Hint(1), player_1)), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))

    // [   ][ 2 ][F.0]
    // [   ][   ][F.1]
    // [   ][F.1][   ]
    val test_board_1 = Board[PlayerTile](3, 3, Map(
      (Coordinate(1,1) -> PlayerTile.Hidden), (Coordinate(1,2) -> PlayerTile.Revealed(SolutionTile.Hint(2))), (Coordinate(1,3) -> PlayerTile.Flagged(player_0)), 
      (Coordinate(2,1) -> PlayerTile.Hidden), (Coordinate(2,2) -> PlayerTile.Hidden), (Coordinate(2,3) -> PlayerTile.Flagged(player_1)), 
      (Coordinate(3,1) -> PlayerTile.Hidden), (Coordinate(3,2) -> PlayerTile.Flagged(player_1)), (Coordinate(3,3) -> PlayerTile.Hidden)
    ))
    
    val test_player_0_1 = Player(PlayerID(0), test_board_0, PlayerColor(PrinterEffects.Red))
    val test_player_1_1 = Player(PlayerID(1), test_board_1, PlayerColor(PrinterEffects.Blue))
    
    assert(num_flagged_non_mines_per_player(PlayerPool(List(test_player_0_1, test_player_1_1), 1), solution_board) == List(0, 1))
  }
*/
}
