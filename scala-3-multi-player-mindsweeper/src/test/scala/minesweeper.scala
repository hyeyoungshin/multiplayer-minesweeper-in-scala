import org.scalatest.funsuite.AnyFunSuite

class Minesweeper extends AnyFunSuite {
  val mine_locations = Array(Array(0, 0, 1), Array(0, 0, 0), Array(0, 0, 1))
  
  val filename = "src/test/board_tests/1-in.json"
  val game_input = parse_game_input(filename)
  val mineboard = create_mineboard(game_input.board)  
  val test_pos = Coordinate(1, 2)
  
  val solutionboard_3x3 = create_solutionboard(mineboard)
  val playerboard_3x3 = create_playerboard(3, 3)
    

  test("count_neighboring_mines") {
    val res = count_neighboring_mines(mineboard, test_pos)
    assert(res == 2)
  }

  test("parse_game_input") {
    val filename = "src/test/board_tests/1-in.json"
    val game_input = parse_game_input(filename)

    assert(game_input.board.length == 3)
    assert(game_input.reveal.head.row == 2)
  }

  test("get_solutiontile_at-hint") {
    val test_tile = get_solutiontile_at(mineboard, test_pos)
    
    assert(test_tile == SolutionTile.Hint(2))
  }

  test("get_solutiontile_at-empty") {
    val test_tile = get_solutiontile_at(mineboard, Coordinate(0, 0))
    
    assert(test_tile == SolutionTile.Empty)
  }

  test("get_solutiontile_at-mine") {
    val test_tile = get_solutiontile_at(mineboard, Coordinate(0, 2))
    
    assert(test_tile == SolutionTile.Mine)
  }

  test("create_solutiboard") {
    val solutionboard = create_solutionboard(mineboard)
    
    assert(solutionboard.tile_map(Coordinate(1, 2)) == SolutionTile.Hint(2))
  }

  test("reveal_neighbors") {
    // [E, 1]
    // [E, 1]
    val solutionboard_2x2 = Board(2, 2, Map((Coordinate(0, 0) -> SolutionTile.Empty), (Coordinate(0, 1) -> SolutionTile.Hint(1)), 
                                  (Coordinate(1, 0) -> SolutionTile.Empty), (Coordinate(1, 1) -> SolutionTile.Hint(1))))
    val playerboard_2x2 = create_playerboard(2, 2)
    // [R(E), H]
    // [H, H]
    val updated_playerboard_2x2 = 
      Board(2, 2, playerboard_2x2.tile_map + (Coordinate(0,0) -> PlayerTile.Revealed(SolutionTile.Empty)))
    
    val test = reveal_neighbors(solutionboard_2x2, updated_playerboard_2x2, Coordinate(0, 0))

    assert(test.tile_map(Coordinate(1, 0)) == PlayerTile.Revealed(SolutionTile.Empty))
    assert(test.tile_map(Coordinate(0, 1)) == PlayerTile.Revealed(SolutionTile.Hint(1)))
  }

  test("reveal-one-tile") {
    val test = reveal(solutionboard_3x3, playerboard_3x3, test_pos)

    assert(test.tile_map(Coordinate(1, 2)) == PlayerTile.Revealed(SolutionTile.Hint(2)))
  }

  test("reveal-more-tiles-if-empty") {
    val test = reveal(solutionboard_3x3, playerboard_3x3, Coordinate(1, 0))

    assert(test.tile_map(Coordinate(0, 0)) == PlayerTile.Revealed(SolutionTile.Empty))
    assert(test.tile_map(Coordinate(0, 1)) == PlayerTile.Revealed(SolutionTile.Hint(1)))
  }

  test("reveal-all-mines-if-hit-mine") {
    val test = reveal(solutionboard_3x3, playerboard_3x3, Coordinate(0, 2))

    assert(test.tile_map(Coordinate(0, 2)) == PlayerTile.Revealed(SolutionTile.Mine))
    assert(test.tile_map(Coordinate(2, 2)) == PlayerTile.Revealed(SolutionTile.Mine))
  }

  test("convert_input_coordinates") {
    val input_coordinates = game_input.reveal
    assert(convert_input_coordinates(input_coordinates).head == test_pos)
  }
}
