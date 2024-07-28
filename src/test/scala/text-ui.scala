import org.scalatest.funsuite.AnyFunSuite

class TextUI extends AnyFunSuite {
  var state = new_game(Easy)

  test("player action") {
    val new_state = play(state, PlayerAction(Action.Reveal, Coordinate(1, 1)))
    val current_player = new_state.player_pool.current()
    assert(current_player.board.tile_map(Coordinate(1, 1)) != PlayerTile(None, None))
  }

}

// should not win
// [E][1][ ]
// [E][2][ ] 
// [E][2][F]