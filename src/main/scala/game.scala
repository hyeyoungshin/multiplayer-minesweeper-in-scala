import scala.io.StdIn.readLine


val BOARD_SIZE = (5, 5)
val NUM_MINES = 5

enum Status:
  case Win
  case Lose
  case Continue

  def print_status: Unit = 
    this match {
      case Win => println("You win!")
      case Lose => println("You lost...")
      case Continue => ()
    }


case class GameState(val solution_board: SolutionBoard, val player_board: PlayerBoard, val status: Status):
  def print_state: Unit = 
    player_board.print_board
    status.print_status


def valid_user_input(state: GameState, user_input: InputCoordinate): Boolean = 
  val tile_pos = convert_input_coordinate(user_input)
  state.player_board.within_boundary(tile_pos) && state.player_board.is_hidden(tile_pos)


def parse_user_input(user_input: String): Option[InputCoordinate] = 
  val int_arr = user_input.split(",").map(_.toInt)
  int_arr.length match {
    case 2 => Some(InputCoordinate(int_arr(0), int_arr(1)))
    case _ => None
  }
  

def new_game(): GameState = 
  val mine_locations = generate_mine_locations(NUM_MINES, BOARD_SIZE)
  val mine_board = create_mineboard(mine_locations)
  val solution_board = create_solutionboard(mine_board)
  val initial_board = create_playerboard(BOARD_SIZE._1, BOARD_SIZE._2)
  
  GameState(solution_board, initial_board, Status.Continue)


def game_over(state: GameState): Boolean = 
  state.status match {
    case Status.Lose => true
    case Status.Win => true
    case Status.Continue => false
  }


def update_state(state: GameState, new_player_board: PlayerBoard, tile_pos: Coordinate): GameState = 
  val new_status = 
    if has_won(state.solution_board, new_player_board) then
      Status.Win
    else
      new_player_board.tile_map(tile_pos) match {
        case PlayerTile.Revealed(SolutionTile.Mine) => Status.Lose
        case PlayerTile.Revealed(SolutionTile.Empty) => Status.Continue
        case PlayerTile.Revealed(SolutionTile.Hint(n)) => Status.Continue
        case PlayerTile.Flagged => Status.Continue
        case _ => throw IllegalStateException("tile cannot be hidden.")
      }
    
  GameState(state.solution_board, new_player_board, new_status)


def play(state: GameState, tile_pos: Coordinate, reveal_or_flag: String): GameState = 
  state.status match {
      case Status.Continue => 
        val cur_playerboard = reveal_or_flag match {
          case "R" => reveal(state.solution_board, state.player_board, tile_pos) 
          case "F" => flag(state.player_board, tile_pos)
          case _ => state.player_board
        }
        update_state(state, cur_playerboard, tile_pos)
      case _ => throw IllegalStateException()
    }


// * Promt user with input request until it is valid to use
// * valid input is within range of the board, fresh, length = 2 
// ** You give
// state: GameState
// ** You get
// a valid user input for a tile position to reveal
def get_valid_input(state: GameState): InputCoordinate = 
  println("Enter a valid tile position separated by a comma: ")
  val user_input = readLine()
  
  val parsed_input = parse_and_validate(state, user_input)
  parsed_input match {
    case Some(valid_input) => valid_input
    case None => get_valid_input(state)
  }


def parse_and_validate(state: GameState, user_input: String): Option[InputCoordinate] = 
  val parsed_input = parse_user_input(user_input)
  parsed_input match {
    case Some(parsed) => valid_user_input(state, parsed) match {
      case true => Some(parsed)
      case false => None
    }
    case None => None
  }


@main def game(): Unit = 

  print_start()

  var state = new_game()

  state.print_state

  while !game_over(state) do 
    val valid_input = get_valid_input(state)
    val tile_pos = convert_input_coordinate(valid_input)

    println("Reveal for R or flag for F.")
    val reveal_or_flag = readLine()
    state = play(state, tile_pos, reveal_or_flag)
    state.print_state


// TODO:
// Separate Game(model) and text ui(view).
// Game - Coordinate || text ui - InputCoordinate