import org.scalatest.funsuite.AnyFunSuite

class Parse extends AnyFunSuite {

  test("parse_game_input") {
        val filename = "src/test/board_tests/1-in.json"
        val game_input = parse_game_input(filename)

        assert(game_input.board.length == 3)
        assert(game_input.reveal.head.row == 2)
  }

}