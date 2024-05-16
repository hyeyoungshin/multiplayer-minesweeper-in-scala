import org.scalatest.funsuite.AnyFunSuite

class Minesweeper extends AnyFunSuite {
  val mine_locations = Array(Array(0, 0, 1), Array(0, 0, 0), Array(0, 0, 1))
  
  val filename = "src/test/board_tests/1-in.json"
  val game_input = parse_game_input(filename)
  val mineboard = create_mineboard(game_input.board)  
  val click = Coordinate(game_input.reveal.head.row - 1, game_input.reveal.head.column - 1)

  val playerboard = Board(
    xsize = 3,
    ysize = 3,
    tile_map = Map(Coordinate(0, 0) -> PlayerTile.Hidden, Coordinate(0, 1) -> PlayerTile.Hidden, Coordinate(0, 2) -> PlayerTile.Hidden,
                   Coordinate(1, 0) -> PlayerTile.Hidden, Coordinate(1, 1) -> PlayerTile.Hidden, Coordinate(1, 2) -> PlayerTile.Hidden,
                   Coordinate(2, 0) -> PlayerTile.Hidden, Coordinate(2, 1) -> PlayerTile.Hidden, Coordinate(2, 2) -> PlayerTile.Hidden
    )
  )

  

  test("count_neighboring_mines") {
    val res = count_neighboring_mines(mineboard, click)
    assert(res == 2)
  }

  test("parse_game_input") {
    val filename = "src/test/board_tests/1-in.json"
    val game_input = parse_game_input(filename)

    assert(game_input.board.length == 3)
    assert(game_input.reveal.head.row == 2)
  }

  test("get_solutiontile_at-hint") {
    val test_tile = get_solutiontile_at(mineboard, click)
    
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

  test("reveal") {
    val solutionboard = create_solutionboard(mineboard)
    val next_playerboard = reveal(solutionboard, playerboard, click)

    assert(next_playerboard.tile_map(click) == PlayerTile.Revealed(SolutionTile.Hint(2)))
  }
}
