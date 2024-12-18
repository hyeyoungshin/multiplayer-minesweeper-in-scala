package minesweeper.game

import org.scalatest.funsuite.AnyFunSuite
import minesweeper.print_board

class GameTest extends AnyFunSuite {
  val mine_locations = Array(Array(0, 0, 1), Array(0, 0, 0), Array(0, 0, 1))
  // [ ][ ][*]
  // [ ][ ][ ]
  // [ ][ ][*]
  val mineboard = create_mineboard_for_test(mine_locations)
  
  // [0][1][x]
  // [0][2][2]
  // [0][1][x]
  val solution_board = create_solutionboard(mineboard)

  val playerboard = create_playerboard(3, 3)
  val player_0 = Player(0)
  val player_1 = Player(1)
  val playerpool = PlayerPool(List(PlayerState(player_0, playerboard), PlayerState(player_1, playerboard)), 0)
  val solution = Solution(solution_board, 2)
/*
  test("new_game-easy") {
    val game_state = new_game(Easy)

    assert(game_state.solution.num_mines == 2)
    assert(game_state.playerpool.current_playerboard().xsize == 3)
    assert(game_state.playerpool.playerstates.length == 2)
    assert(game_state.status == GameStatus.Continue)
  }

  test("play") {
    val old_state = GameState(solution,
                              playerpool,
                              GameStatus.Continue)
    val player_action = PlayerAction(Action.Reveal, Coordinate(0,2))
    val new_state = play(old_state, player_action)
    
    assert(new_state.playerpool.current_player().id == 0)
    assert(new_state.playerpool.current == 0)
    assert(new_state.playerpool.current_playerboard().tile_map(Coordinate(0,2)) == PlayerTile.Revealed(SolutionTile.Empty))
    assert(new_state.playerpool.current_playerboard().tile_map(Coordinate(1,2)) == PlayerTile.Revealed(SolutionTile.Hint(1)))
  }

  test("update_player") {
    val updated_playerpool = update_player(playerpool, reveal(solution_board, Coordinate(1,0)))

    assert(playerpool.current_playerboard().tile_map(Coordinate(1,0)) == PlayerTile.Hidden)
    assert(updated_playerpool.current_playerboard().tile_map(Coordinate(1,0)) == PlayerTile.Revealed(SolutionTile.Hint(1)))
    assert(updated_playerpool.current == 0)
  }

  test("update_playerpool") {
    val player_1_reveal = PlayerState(player_1, reveal(solution_board, Coordinate(1,2))(playerboard))
    val playerpool_new = PlayerPool(List(PlayerState(player_0, playerboard), player_1_reveal), 0)
    
    val updated_playerpool = update_playerpool(playerpool_new, flag(playerpool.current_player(), Coordinate(2,0)))
    
    assert(playerpool.current_playerboard().tile_map(Coordinate(2,0)) == PlayerTile.Hidden)
    assert(updated_playerpool.current_player().id == 0)
    assert(updated_playerpool.current_playerboard().tile_map(Coordinate(2,0)) == PlayerTile.Flagged(playerpool.current_player()))
    assert(updated_playerpool.playerstates(1).board.tile_map(Coordinate(2,0)) == PlayerTile.Flagged(playerpool.current_player()))
  }

  // test("win_or_lose-hit_mine") {
  //   val player_1_new = player_1.copy(board = reveal(solution_board, Coordinate(1,2))(player_1.board))
  //   val playerpool_new = PlayerPool(List(player_0, player_1_new), 0)

  //   val updated_playerpool = update_player(playerpool_new, reveal(solution_board, Coordinate(2,0)))
    
  //   val test = win_or_lose(Solution(solution_board, 2), updated_playerpool, Coordinate(2,0))
 
  //   assert(test.status match {
  //     case GameStatus.Win(winners) => true
  //     case GameStatus.Continue => false
  //   })
  // }

  test("who_won-one-winner") {
    val pre_winning_board = update_player_board(playerboard, Coordinate(2,0), PlayerTile.Flagged(player_0))
    val winning_board = update_player_board(pre_winning_board, Coordinate(2,2), PlayerTile.Flagged(player_0))
    val winner = PlayerState(Player(0, Red), winning_board)
    val test_playerpool = PlayerPool(List(winner, PlayerState(player_1, playerboard)), 0)
        
    assert(who_won(test_playerpool, solution_board) == Set(winner.player))
  }

  test("who_won-tie") {
    val winning_board_0 = update_player_board(playerboard, Coordinate(2,0), PlayerTile.Flagged(player_0))
    val winning_board_1 = update_player_board(playerboard, Coordinate(2,2), PlayerTile.Flagged(player_1))
    val winner_0 = PlayerState(Player(0, Red), winning_board_0)
    val winner_1 = PlayerState(Player(1, Blue), winning_board_1)
    val test_playerpool = PlayerPool(List(winner_0, winner_1), 1)
    
    assert(who_won(test_playerpool, solution_board) == Set(winner_0.player, winner_1.player))
  }

  test("get_points") {
    val board_1 = update_player_board(playerboard, Coordinate(2,0), PlayerTile.Flagged(player_0)) // +1
    val board_2 = update_player_board(board_1, Coordinate(2,2), PlayerTile.Flagged(player_0)) // +1
    val board_3 = update_player_board(board_2, Coordinate(0,2), PlayerTile.Flagged(player_0)) // -1
    val test_player = PlayerState(Player(0, Red), board_3) 
    val mine_locations = mine_coordinates(solution_board)
    
    assert(get_points(test_player, mine_locations) == 1)
  }

  test("get_points-0") {
    // [ ][ ][F]
    // [ ][ ][ ]
    // [F][F][F]
    val board_1 = update_player_board(playerboard, Coordinate(2,0), PlayerTile.Flagged(player_0)) // +1
    val board_2 = update_player_board(board_1, Coordinate(2,2), PlayerTile.Flagged(player_0)) // +1
    val board_3 = update_player_board(board_2, Coordinate(0,2), PlayerTile.Flagged(player_0)) // -1
    val board_4 = update_player_board(board_3, Coordinate(1,2), PlayerTile.Flagged(player_0)) // -1
    val test_player = PlayerState(Player(0, Red), board_4)
    val mine_locations = mine_coordinates(solution_board)
    
    assert(get_points(test_player, mine_locations) == 0)
  }

  test("has_won-with_get_points") {
    val board_0_1 = update_player_board(playerboard, Coordinate(2,0), PlayerTile.Flagged(player_0)) // +1
    val board_1_1 = update_player_board(playerboard, Coordinate(2,2), PlayerTile.Flagged(player_1)) // +1
    val board_1_2 = update_player_board(board_1_1, Coordinate(0,2), PlayerTile.Flagged(player_1)) // -1
    val test_player_0 = PlayerState(Player(0, Red), board_0_1)
    val test_player_1 = PlayerState(Player(1, Blue), board_1_2)
    val test_playerpool = PlayerPool(List(test_player_0, test_player_1), 0)
        
    assert(who_won(test_playerpool, solution_board) == Set(test_player_0.player))
  }

  test("has_won-with_get_points-tie") {
    val board_0_1 = update_player_board(playerboard, Coordinate(2,0), PlayerTile.Flagged(player_0)) // +1
    val board_1_1 = update_player_board(playerboard, Coordinate(2,2), PlayerTile.Flagged(player_1)) // +1
    val test_player_0 = PlayerState(Player(0, Red), board_0_1)
    val test_player_1 = PlayerState(Player(1, Blue), board_1_1)
    val test_playerpool = PlayerPool(List(test_player_0, test_player_1), 1)
    who_won(test_playerpool, solution_board).map(player => println(s"winner is: ${player.id}"))
        
    assert(who_won(test_playerpool, solution_board) == Set(test_player_0.player, test_player_1.player))
  }
*/
  // [0][1][x]
  // [0][2][2]
  // [0][1][x]

  test("reveal-mines-with-flags") {
    val state_0 = GameState(solution,
                              playerpool,
                              GameStatus.Continue)
    val player_0_action = PlayerAction(Action.Flag, Coordinate(1,1))
    val state_1 = play(state_0, player_0_action)
    print_board(state_1.playerpool.current_playerboard())
    // [ ][ ][ ]
    // [ ][F][ ]
    // [ ][ ][ ]
    val state_2 = next_player(state_1)
    val player_1_action = PlayerAction(Action.Reveal, Coordinate(2, 0))
    val state_3 = play(state_2, player_1_action)
    print_board(state_3.playerpool.current_playerboard())
  }

}