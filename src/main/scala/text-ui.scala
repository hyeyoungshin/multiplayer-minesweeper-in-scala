import scala.io.StdIn.readLine

@main def text_ui_game(): Unit = 
  print_start()

  var state = new_game()

  print_state(state)

  while !game_over(state) do 
    val valid_input_coordinate = get_valid_input_coordinate(state)
    val tile_pos = convert_input_coordinate(valid_input_coordinate)
    val valid_player_action = get_valid_player_action(state, tile_pos)

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


//////////////////////
// input coordinate //
//////////////////////

// Promt user with input request until it is valid
// valid input is a tile position within range of the board
def get_valid_input_coordinate(state: GameState): InputCoordinate = 
  println("Enter a tile position: ")
  val player_input = readLine()
  
  val parsed_and_validated = parse_and_validate(state, player_input)
  parsed_and_validated match {
    case Some(input_coordinate) => input_coordinate
    case None => get_valid_input_coordinate(state)
  }


def parse_and_validate(state: GameState, user_input: String): Option[InputCoordinate] = 
  val parsed_input_coordinate = parse_user_input(user_input)
  parsed_input_coordinate match {
    case Some(input_coordinate) => valid_user_input(state, input_coordinate) match {
      case true => Some(input_coordinate)
      case false => None
    }
    case None => None
  }


/* parse user input via int array
for example, 1,1 is [1,1]
int array of length other than 2 is considered invalid
*/ 
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


/* handle cases where user type non numeric coordinates */
def parse_user_input_helper(user_input: String): Option[Array[Int]] = 
  try {
    Some(user_input.split(",").map(_.toInt))
  } catch {
    case _: NumberFormatException => None
  }


/* check user input coordinate is valid
user input coordinate is valid when it is within board boundary AND
the tile at input coordinate is not revealed
 */
def valid_user_input(state: GameState, user_input: InputCoordinate): Boolean = 
  val tile_pos = convert_input_coordinate(user_input)
  state.player_board.within_boundary(tile_pos) && 
  ((state.player_board.tile_map(tile_pos) == PlayerTile.Hidden) || 
   (state.player_board.tile_map(tile_pos) == PlayerTile.Flagged))


///////////////////
// player action //
///////////////////

def get_valid_player_action(state: GameState, pos: Coordinate): PlayerAction = 
  println("Enter an action. R for reveal, F for flag, U for unflag: ")
  val input = readLine()
  val parsed_and_validated = parse_and_validate_player_action(state, input, pos)
  
  parsed_and_validated match {
    case Some(player_action) => player_action
    case None => get_valid_player_action(state, pos)
  }


def parse_and_validate_player_action(state: GameState, input: String, pos: Coordinate): Option[PlayerAction] = 
  val parsed_action = parse_player_action(input, pos)

  parsed_action match {
    case Some(action) => valid_player_action(state, pos, action) match {
      case true => Some(action)
      case false => None
    }
    case None => None
  }


def parse_player_action(input: String, pos: Coordinate): Option[PlayerAction] = 
  input match {
    case "R" => Some(PlayerAction.Reveal(pos))
    case "F" => Some(PlayerAction.Flag(pos))
    case "U" => Some(PlayerAction.Unflag(pos))
    case _ => None
  }


def valid_player_action(state: GameState, pos: Coordinate, action: PlayerAction): Boolean = 
  val playertile = state.player_board.tile_map(pos)

  action match {
    case PlayerAction.Flag(_) => playertile == PlayerTile.Hidden
    case PlayerAction.Reveal(_) => playertile == PlayerTile.Hidden
    case PlayerAction.Unflag(_) => playertile == PlayerTile.Flagged
  }




// def print_action(state: GameState, action: PlayerAction): Unit = 
//   val playertile = state.player_board.tile_map(action.extract_pos) 
  
//   playertile match {
//     case PlayerTile.Flagged => action match {
//       case PlayerAction.Flag(_) => println(s"You can't flag a tile that's been flagged already.") 
//       case PlayerAction.Unflag(_) => println("")
//       case PlayerAction.Reveal(_) => println(s"You can't flag a tile that's been revealed.")
//     }
//     case PlayerTile.Hidden => action match {
//       case PlayerAction.Flag(pos) => println(s"You can't reveal a tile that's been flagged.") 
//       case PlayerAction.Unflag(pos) => println(s"You can't unflag a tile that's hidden.") 
//       case PlayerAction.Reveal(pos) => println("")
//     }
//     case PlayerTile.Revealed(_) => action match {
//       case PlayerAction.Flag(pos) => println(s"You can't flag a tile that's been revealed.") 
//       case PlayerAction.Unflag(pos) => println(s"You can't unflag a tile that's been revealed.") 
//       case PlayerAction.Reveal(pos) => println(s"You can't reveal a tile that's been revealed already.")
//     }
//   }