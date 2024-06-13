import org.scalatest.funsuite.AnyFunSuite

class TextUI extends AnyFunSuite {
  var state = new_game()

  test("player action") {
    val next_state = play(state, PlayerAction.Reveal(Coordinate(1, 1)))
    assert(next_state.player_board.tile_map(Coordinate(1,1)) != PlayerTile.Hidden)
  }

}