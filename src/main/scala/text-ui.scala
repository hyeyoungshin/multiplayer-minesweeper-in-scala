import scala.io.StdIn.readLine

@main def text_ui_game(): Unit = 
  print_start()

  var state = new_game()

  print_state(state)

  while !game_over(state) do 
    val valid_input_coordinate = get_valid_input_coordinate(state)
    val tile_pos = convert_input_coordinate(valid_input_coordinate)
    val valid_player_action = get_valid_player_action(tile_pos)

    state = play(state, valid_player_action)
    print_state(state)


def print_start(): Unit = 
  println("\n")
  println("Welcome to the minesweeper game. \n")
  println("Enter your name: ")
  val user_name = readLine()
  println("\n")
  println(s"Hello, $user_name! Starting a game with \n")
  println(s"Board size: $BOARD_SIZE \n")
  println(s"Number of mines: $NUM_MINES \n")


def print_status(status: GameStatus): Unit = 
  status match {
    case GameStatus.Win => println("You win!")
    case GameStatus.Lose => println("You lost...")
    case GameStatus.Continue => ()
  }


def print_state(state: GameState): Unit = 
    state.player_board.print_board
    print_status(state.status)


// * Promt user with input request until it is valid
// * valid input is within range of the board, fresh, a pair of int
// ** You give
// state: GameState
// ** You get
// a valid user input for a tile position to revearl
def get_valid_input_coordinate(state: GameState): InputCoordinate = 
  println("Enter a tile position: ")
  val player_input = readLine()
  
  val parsed_input = parse_and_validate(state, player_input)
  parsed_input match {
    case Some(valid_input) => valid_input
    case None => get_valid_input_coordinate(state)
  }


def get_valid_player_action(pos: Coordinate): PlayerAction = 
  println("Enter an action. R for reveal, F for flag, U for unflag: ")
  val player_input = readLine()
  
  player_input match {
    case "R" => PlayerAction.Reveal(pos)
    case "F" => PlayerAction.Flag(pos)
    case "U" => PlayerAction.Unflag(pos)
    case _ => get_valid_player_action(pos)
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


def parse_user_input(user_input: String): Option[InputCoordinate] =
  val option_parsed = parse_user_input_helper(user_input)

  option_parsed match {
    case Some(int_arr) => { 
      int_arr.length match {
        case 2 => Some(InputCoordinate(int_arr(0), int_arr(1)))
        case _ => None
      }
    }
    case None => None
  }


def parse_user_input_helper(user_input: String): Option[Array[Int]] = 
  try {
    Some(user_input.split(",").map(_.toInt))
  } catch {
    case _: NumberFormatException => None
  }


def valid_user_input(state: GameState, user_input: InputCoordinate): Boolean = 
  val tile_pos = convert_input_coordinate(user_input)
  state.player_board.within_boundary(tile_pos) // && state.player_board.is_hidden(tile_pos)
