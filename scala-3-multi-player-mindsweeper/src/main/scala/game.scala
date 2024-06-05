import scala.io.StdIn.readLine
import scala.util.Random

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


case class MS_GameState(val solution_board: SolutionBoard, val player_board: PlayerBoard, val status: Status):
  def print_state: Unit = 
    player_board.print_board
    status.print_status
    

def within_boundary(user_input: InputCoordinate): Boolean = 
  user_input.row > 0 && user_input.column > 0 && user_input.row <= BOARD_SIZE._1 && user_input.column <= BOARD_SIZE._2


def fresh(state: MS_GameState, user_input: InputCoordinate): Boolean = 
  state.player_board.tile_map(convert_input_coordinate(user_input)) == PlayerTile.Hidden


def valid_user_input(state: MS_GameState, user_input: InputCoordinate): Boolean = 
  within_boundary(user_input) && fresh(state, user_input)


def parse_user_input(user_input: String): Option[InputCoordinate] = 
  val int_arr = user_input.split(",").map(_.toInt)
  int_arr.length match {
    case 2 => Some(InputCoordinate(int_arr(0), int_arr(1)))
    case _ => None
  }


def generate_mine_locations(num_mines: Int, board_size: (Int, Int)): Array[Array[Int]] =
  var board = Array.fill(board_size._1)(Array.fill(board_size._2)(0))
  val random_coordinates = Random.shuffle(generate_coordinate_keys(board_size._1, board_size._2))
  
  val mine_locations = random_coordinates.take(num_mines)
  mine_locations.foreach((x, y) => board(x)(y) = 1)
  board
  

def new_game(): MS_GameState = 
  val mine_locations = generate_mine_locations(NUM_MINES, BOARD_SIZE)
  val mine_board = create_mineboard(mine_locations)
  val solution_board = create_solutionboard(mine_board)
  val initial_board = create_playerboard(BOARD_SIZE._1, BOARD_SIZE._2)
  
  MS_GameState(solution_board, initial_board, Status.Continue)


def game_over(state: MS_GameState): Boolean = 
  state.status match {
    case Status.Lose => true
    case Status.Win => true
    case Status.Continue => false
  }


def win(solution_board: SolutionBoard, player_board: PlayerBoard): Boolean = 
  val hidden_pos = player_board.tile_map.filter((pos, tile) => tile == PlayerTile.Hidden).keys
  hidden_pos.foldLeft(true)((acc, pos) => acc && solution_board.tile_map(pos) == SolutionTile.Mine)


def update_state(state: MS_GameState, new_player_board: PlayerBoard, tile_pos: Coordinate): MS_GameState = 
  val new_status = 
    if win(state.solution_board, new_player_board) then
      Status.Win
    else
      new_player_board.tile_map(tile_pos) match {
        case PlayerTile.Revealed(SolutionTile.Mine) => Status.Lose
        case PlayerTile.Revealed(SolutionTile.Empty) => Status.Continue
        case PlayerTile.Revealed(SolutionTile.Hint(n)) => Status.Continue
        case _ => throw IllegalStateException("tile cannot be hidden.")
      }
    
  MS_GameState(state.solution_board, new_player_board, new_status)

  
def print_start(): Unit = 
  println("\n")
  println("Welcome to the minesweeper game. \n")
  println("Enter your name: ")
  val user_name = readLine()
  println("\n")
  println(s"Hello, $user_name! Starting a game with \n")
  println(s"Board size: $BOARD_SIZE \n")
  println(s"Number of mines: $NUM_MINES \n")


def play(state: MS_GameState, tile_pos: Coordinate): MS_GameState = 
  state.status match {
      case Status.Continue => 
        val cur_playerboard = reveal(state.solution_board, state.player_board, tile_pos) 
        
        update_state(state, cur_playerboard, tile_pos)
      case _ => throw IllegalStateException()
    }


// promt user with input request until it is valid to use
// valid input is within range of the board, input is fresh, length should be 2 
//
def get_valid_input(state: MS_GameState): InputCoordinate = 
  println("Enter a valid tile position separated by a comma: ")
  val user_input = readLine()
  
  val parsed_input = parse_and_validate(state, user_input)
  parsed_input match {
    case Some(valid_input) => valid_input
    case None => get_valid_input(state)
  }


def parse_and_validate(state: MS_GameState, user_input: String): Option[InputCoordinate] = 
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

    state = play(state, tile_pos)
    state.print_state

