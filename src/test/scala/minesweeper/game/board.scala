package minesweeper.game

import org.scalatest.funsuite.AnyFunSuite

class BoardTest extends AnyFunSuite {
  // [0][0][1]
  // [0][0][0]
  // [0][0][1]
  val mine_locations = Array(Array(0, 0, 1), Array(0, 0, 0), Array(0, 0, 1))
  
  // val filename = "src/test/board_tests/1-in.json"
  // val game_input = parse_game_input(filename)
  // val mineboard = create_mineboard_for_test(game_input.board)
  // val test_pos = Coordinate(1, 2)
  
  // val solutionboard_3x3 = create_solutionboard(mineboard)
  // val playerboard_3x3 = create_playerboard(3, 3)

  val mineboard = create_mineboard_for_test(mine_locations)
  val mine_location_1 = Coordinate(2, 0) // (0, 2)
  val mine_location_2 = Coordinate(2, 2) // (2, 2)

  // [E][1][x]
  // [E][2][2]
  // [E][1][x]
  val solution_board = create_solutionboard(mineboard)

  test("create_mineboard_for_test") {
    val mineboard = create_mineboard_for_test(mine_locations)
    val mine_location_1 = Coordinate(2, 0) // (0, 2)
    val mine_location_2 = Coordinate(2, 2) // (2, 2)
    assert(mineboard.tile_map(mine_location_1))
    assert(mineboard.tile_map(mine_location_2))
  }

  test("create_mineboard_for_game-easy") {
    val easy_mineboard = create_mineboard_for_game(Easy)
    val num_of_mines_in_easy_mineboard = easy_mineboard.tile_map.foldRight(0)((x, acc) => if x._2 then acc + 1 else acc)
    // test: easy games should have two mines
    assert(num_of_mines_in_easy_mineboard == 2)
  }

  test("create_solution_board") {
    // [E][1][x]
    // [E][2][2]
    // [E][1][x]
    val solution_board = create_solutionboard(mineboard)
    
    assert(solution_board.tile_map(mine_location_1) == SolutionTile.Mine)
    assert(solution_board.tile_map(mine_location_2) == SolutionTile.Mine)  
    assert(solution_board.tile_map(Coordinate(0,0)) == SolutionTile.Empty)  
  }

  test("create_player_board") {
    val player_board = create_playerboard(mineboard.xsize, mineboard.ysize)
    
    assert(player_board.tile_map(mine_location_1) == PlayerTile.Hidden)
    assert(player_board.tile_map(mine_location_2) == PlayerTile.Hidden)  
  }
    
  test("count_neighboring_mines") {
    assert(count_neighboring_mines(mineboard, Coordinate(0,0)) == 0)
    assert(count_neighboring_mines(mineboard, Coordinate(1,0)) == 1)
    assert(count_neighboring_mines(mineboard, Coordinate(1,1)) == 2)
  }

  test("update_player_board") {
    val player_board = create_playerboard(mineboard.xsize, mineboard.ysize)
    val new_playerboard = update_player_board(player_board, Coordinate(0,0), PlayerTile.Revealed(SolutionTile.Empty))

    assert(player_board.tile_map(Coordinate(0,0)) == PlayerTile.Hidden)    
    assert(new_playerboard.tile_map(Coordinate(0,0)) == PlayerTile.Revealed(SolutionTile.Empty))    
  }

  // [E][1][x]
  // [E][2][2]
  // [E][1][x]
  test("reveal-hint") {
    val player_board = create_playerboard(mineboard.xsize, mineboard.ysize)
    val next_player_board = reveal(solution_board, Coordinate(2,1))(player_board)

    assert(next_player_board.tile_map(Coordinate(2,1)) == PlayerTile.Revealed(SolutionTile.Hint(2)))        
    assert(next_player_board.tile_map(Coordinate(2,2)) == PlayerTile.Hidden)        
  }

  test("reveal-empty") {
    val player_board = create_playerboard(mineboard.xsize, mineboard.ysize)
    val next_player_board = reveal(solution_board, Coordinate(0,0))(player_board)
    
    assert(next_player_board.tile_map(Coordinate(1,0)) == PlayerTile.Revealed(SolutionTile.Hint(1)))    
    assert(next_player_board.tile_map(Coordinate(1,1)) == PlayerTile.Revealed(SolutionTile.Hint(2)))    
  }
}
