////////////////////////////
///// Data types: Game /////
////////////////////////////
case class GameState (val solution_board: SolutionBoard, 
                      val game_board: GameBoard, 
                      val status: GameStatus)

enum GameStatus:
  case Win 
  case Lose 
  case Continue(next: Player)

enum GameMode:
  case Single
  case Multi(how_many: Int)

case class GameDifficulty(val size: (Int, Int), val num_mines: Int)
final val Easy = GameDifficulty((3, 3), 2)
final val Medium = GameDifficulty((5, 5), 4)
final val Hard = GameDifficulty((7, 7), 9)

case class GameBoard(val player_board: PlayerBoard, val num_mines: Int)

//////////////////////////////
///// Data types: Player /////
//////////////////////////////

case class Player(val name: String, val board: PlayerBoard, val num_flags: Int)

case class PlayerAction(val action: Action, val pos: Coordinate)

enum Action:
  case Reveal, Flag, Unflag


//////////////////////////////
///////// Game Logic /////////
//////////////////////////////
def new_game(difficulty: GameDifficulty): GameState = 
  val mine_board = generate_mine_locations(difficulty)
  val solution_board = create_solutionboard(mine_board)
  val initial_board = create_playerboard(mine_board.xsize, mine_board.ysize)
  
  GameState(solution_board, GameBoard(initial_board, difficulty.num_mines), GameStatus.Continue(Player("", initial_board, 0)))


def game_over(state: GameState): Boolean = 
  state.status match {
    case GameStatus.Lose => true
    case GameStatus.Win => true
    case GameStatus.Continue => false
  }


def play(state: GameState, player_action: PlayerAction): GameState = 
  state.status match {
      case GameStatus.Continue => {
        val new_player_board = (player_action.action, player_action.pos) match {
          case (Action.Reveal, pos) => Some(reveal(state.solution_board, state.game_board.player_board, pos))
          case (Action.Flag, pos) => flag(state.game_board.player_board, pos)
          case (Action.Unflag, pos) => unflag(state.game_board.player_board, pos)
        }

        new_player_board match {
          case Some(playerboard) => update_state(state, GameBoard(playerboard, state.game_board.num_mines), player_action.pos)
          case None => ??? // get_valid_input()
        }
      }
      case _ => throw IllegalStateException("You can only play game in Continue Status.")
    }


/* player wins if they find all mines which means
number of hidden playertiles equals number of mines AND
number of flagged playertiles is 0
*/
def has_won(solution_board: SolutionBoard, player_board: PlayerBoard): Boolean = 
  val num_mines = solution_board.tile_map.count((_, tile) => tile == SolutionTile.Mine)
  val num_hidden = player_board.tile_map.count((_, tile) => tile == PlayerTile.Hidden)
  val num_flagged = player_board.tile_map.count((_, tile) => tile == PlayerTile.Flagged)

  num_hidden == num_mines && num_flagged == 0


def update_state(state: GameState, game_board: GameBoard, tile_pos: Coordinate): GameState = 
  val new_status = 
    if has_won(state.solution_board, game_board.player_board) then
      GameStatus.Win
    else
      game_board.player_board.tile_map(tile_pos) match {
        case PlayerTile.Revealed(SolutionTile.Mine) => GameStatus.Lose
        case PlayerTile.Revealed(SolutionTile.Empty) => GameStatus.Continue(Player("dummy", game_board.player_board, 0))
        case PlayerTile.Revealed(SolutionTile.Hint(_)) => GameStatus.Continue(Player("dummy", game_board.player_board, 0))
        case PlayerTile.Flagged => GameStatus.Continue(Player("dummy", game_board.player_board, 0))
        case PlayerTile.Hidden => GameStatus.Continue(Player("dummy", game_board.player_board, 0))
      }
    
  GameState(state.solution_board, game_board, new_status)


// KEEP IN MIND:
// Separate Game(model) and text ui(view).
// Game - Coordinate || text ui - InputCoordinate
// To clean up, we rebuilt via Metals
// Had nested sbt files 


// // tiles and mines ratio
// enum Difficulty:
//   case Easy //  12.6%
//   case Intermediate // 18.1%
//   case Expert // 20.6%


// def flagged_equals_mines(solution_board: SolutionBoard, player_board: PlayerBoard): Boolean = 
  // val mines_pos = solution_board.tile_map.filter((pos, tile) => tile == SolutionTile.Mine).keys
  // mines_pos.foldLeft(true)((acc, pos) => acc && player_board.tile_map(pos) == PlayerTile.Flagged
  
