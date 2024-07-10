import scala.io.StdIn.readLine
import java.awt.Color

object ColorPrinter:
  val Reset = "\u001b[0m"
  val Red = "\u001b[31m"
  val Blue = "\u001b[34m"
  val Green = "\u001b[32m"

  def print_in_color(text: String, color: String): Unit = {
    println(s"$color$text$Reset")
  }

@main def text_ui_game(): Unit = 
  print_start()
  // Not using make_get_valid_input because it does not require `state` and `validate`
  val difficulty = get_valid_difficulty()
  print_difficulty(difficulty)

  var state = new_game(difficulty)
  print_state(state)

  while !game_over(state) do 
    val coordinate = get_valid_coordinate(state)
    // val coordinate = convert_input_coordinate(input_coordinate)
    // Not using make_get_valid_input because coordinate is necessary for parsing and validating
    val player_action = get_valid_action(state, coordinate)
    state = play(state, player_action, coordinate)
    print_state(state)


////////////////
// Game Start //
////////////////  
def print_start(): Unit = 
  println("")
  ColorPrinter.print_in_color("Welcome to the minesweeper game.\n", ColorPrinter.Blue)
  ColorPrinter.print_in_color("Enter your name: ", ColorPrinter.Blue)
  val user_name = readLine()
  ColorPrinter.print_in_color(s"Hello, $user_name!\n", ColorPrinter.Blue)
  println("")


/////////////////////
// Game Difficulty //
/////////////////////  
def get_valid_difficulty(): GameDifficulty = 
  ColorPrinter.print_in_color("Choose difficulty: (Easy, Intermediate, Hard)", ColorPrinter.Blue)
  val user_input = readLine()
  val parsed = parse_difficulty(user_input)
  
  parsed match {
    case Right(difficulty) => difficulty
    case Left(msg) => ColorPrinter.print_in_color(msg, ColorPrinter.Red); get_valid_difficulty()
  }

def parse_difficulty(user_input: String): Either[String, GameDifficulty] = 
  user_input match {
    case "Easy" => Right(Easy)
    case "Intermediate" => Right(Medium)
    case "Hard" => Right(Hard)
    case _ => Left("Enter Easy, Intermediate, or Hard only.")
  }

def print_difficulty(difficulty: GameDifficulty): Unit = 
  println("")
  ColorPrinter.print_in_color(s"Starting a game with board size: (${difficulty.size._1} x ${difficulty.size._2}) " +
    s"and Number of mines: ${difficulty.num_mines}", ColorPrinter.Blue)
  println("")


////////////////
// Game State //
////////////////
def print_state(state: GameState): Unit = 
    state.player_board.print_board
    print_status(state.status)

def print_status(status: GameStatus): Unit = 
  status match {
    case GameStatus.Win => println("You win!")
    case GameStatus.Lose => println("You lost...")
    case GameStatus.Continue => ()
  }


//////////////////////
// Input Coordinate //
//////////////////////
def parse_and_validate_coordinate(user_input: String, state: GameState): Either[String, Coordinate] = 
  parse_coordinate(user_input).flatMap(x => valid_coordinate(state, x))


def make_get_valid_input[T](message: String, parse_and_validate: (String, GameState) => Either[String, T])
: GameState => T = 
  def get_valid_input(state: GameState): T = 
    ColorPrinter.print_in_color(message, ColorPrinter.Blue)
    val player_input = readLine()
    parse_and_validate(player_input, state) match {
      case Right(value) => value
      case Left(msg) => ColorPrinter.print_in_color(msg, ColorPrinter.Red); get_valid_input(state)
    }
  
  get_valid_input

val get_valid_coordinate = make_get_valid_input(message = "Enter a tile position:",
                                                parse_and_validate = parse_and_validate_coordinate)

                                          
                                                

/* parse user input via int array
for example, 1,1 is [1,1]
int array of length other than 2 is considered invalid
*/ 
def parse_coordinate(user_input: String): Either[String, Coordinate] =
  val option_parsed = parse_coordinate_helper(user_input)
  option_parsed.filter(arr => arr.length == 2) match {
    case Some(arr) => Right(convert_input_coordinate(InputCoordinate(arr(0), arr(1))))
    case None => Left("The tile pos is in wrong format.")
  }

/* handle cases where user type non numeric coordinates */
def parse_coordinate_helper(user_input: String): Option[Array[Int]] = 
  try {
    Some(user_input.split(",").map(_.toInt))
  } catch {
    case _: NumberFormatException => None
  }

/* check user input coordinate is valid
user input coordinate is valid when it is within board boundary AND
the tile at input coordinate is not revealed
 */
def valid_coordinate(state: GameState, tile_pos: Coordinate): Either[String, Coordinate] = 
  if state.player_board.within_boundary(tile_pos) then
    state.player_board.tile_map(tile_pos) match {
      case PlayerTile.Revealed(_) => Left("The tile is alreay revealed.")
      case _ => Right(tile_pos)
    }
  else 
    Left("The tile position is outside the boundary.")


///////////////////
// Player Action //
///////////////////
// val get_valid_action = make_get_valid_input(message = "Enter an action: R for reveal, F for flag, U for unflag",
//                                             parse_and_validate = ???)

def get_valid_action(state: GameState, pos: Coordinate): PlayerAction = 
  ColorPrinter.print_in_color("Enter an action: R for reveal, F for flag, U for unflag", ColorPrinter.Blue)
  val input = readLine()
  val parsed_and_validated = parse_action(input, pos).flatMap(x => valid_action(state, x))
  
  parsed_and_validated match {
    case Right(player_action) => player_action
    case Left(msg) => ColorPrinter.print_in_color(msg, ColorPrinter.Red); get_valid_action(state, pos)
  }

def parse_action(input: String, pos: Coordinate): Either[String, PlayerAction] = 
  input match {
    case "R" => Right(PlayerAction.Reveal(pos))
    case "F" => Right(PlayerAction.Flag(pos))
    case "U" => Right(PlayerAction.Unflag(pos))
    case _ => Left("Please only enter R, F, or U: ")
  }

def valid_action(state: GameState, action: PlayerAction): Either[String, PlayerAction] = 
  val playertile = state.player_board.tile_map(action.get_pos())

  action match {
    case PlayerAction.Flag(_) => {
      if playertile == PlayerTile.Hidden then 
        Right(action) 
      else 
        Left("You cannot flag a tile that's already revealed or flagged.\n")
      }
    case PlayerAction.Reveal(_) => {
      if playertile == PlayerTile.Hidden then 
        Right(action) 
      else 
        Left("You cannot reveal a tile that's already revealed or flagged.\n")
      }
    case PlayerAction.Unflag(_) => {
      if playertile == PlayerTile.Flagged then 
        Right(action) 
      else 
        Left("You cannot unflag a tile that's not flagged.\n")
    }
  }



  // judge_action(action, playertile) match {
  //   case Left(bool) => bool
  //   case Right(str) => ColorPrinter.print_in_color(str, ColorPrinter.Red); false // is there another way to do this?
  // }  

// Rust style Result type
// enum ValidationResult:
//   case Ok(result: PlayerAction)
//   case Error(message: String)

// enum Result[T,E]:
//   case Ok(result: T)
//   case Error(message: E)
