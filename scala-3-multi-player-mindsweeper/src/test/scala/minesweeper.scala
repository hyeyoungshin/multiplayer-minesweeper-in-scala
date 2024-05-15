import org.scalatest.funsuite.AnyFunSuite

class Minesweeper extends AnyFunSuite {
  val mine_locations = Array(Array(0, 0, 1), Array(0, 0, 0), Array(0, 0, 1))

  val filename = "src/test/board_tests/1-in.json"
  val game_input = parse_game(filename)

  test("count_mine") {
    val res = count_mine(mine_locations, 1, 2)
    assert(res == 2)
  }

  test("parse_game") {
    val filename = "src/test/board_tests/1-in.json"
    val game_input = parse_game(filename)

    assert(game_input.board.length == 3)
    assert(game_input.reveal.head.row == 2)
  }

  test("create_tile-hint") {
    val loc = (1, 2)
    val test_tile = create_tile(mine_locations, loc._1, loc._2)
    
    assert(test_tile == SolutionTile.Hint(2))
  }

  test("create_tile-empty") {
    val loc = (0, 0)
    val test_tile = create_tile(mine_locations, loc._1, loc._2)
    
    assert(test_tile == SolutionTile.Empty)
  }

  test("create_tile-mine") {
    val loc = (0, 2)
    val test_tile = create_tile(mine_locations, loc._1, loc._2)
    
    assert(test_tile == SolutionTile.Mine)
  }

  test("create_board") {
    
    assert(game_input.board.length == 3)
    assert(game_input.reveal.head.row == 2)
  }

  test("reveal") {
    val filename = "src/test/board_tests/1-in.json"
    val game_input = parse_game(filename)

    assert(game_input.board.length == 3)
    assert(game_input.reveal.head.row == 2)
  }
}
