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

///////////////////////////////////////////////
////////////////// Game Play //////////////////
///////////////////////////////////////////////
@main def text_ui_game(): Unit = 
  print_start()
  val difficulty = get_valid_difficulty()
  print_difficulty(difficulty)

  var state = new_game(difficulty)
  print_state(state)

  while !game_over(state) do 
    val coordinate = get_valid_coordinate(state)
    val player_action = get_valid_action(state)(coordinate)
    state = play(state, player_action)
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
  get_valid_input("Choose difficulty: (Easy, Intermediate, Hard)", parse_difficulty)


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
  print_board(state.player_board)
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

def get_valid_input[T](message: String, parse_and_validate: String => Either[String, T])
: T = 
  def helper(): T = 
    ColorPrinter.print_in_color(message, ColorPrinter.Blue)
    val player_input = readLine()
    parse_and_validate(player_input) match {
      case Right(value) => value
      case Left(msg) => ColorPrinter.print_in_color(msg, ColorPrinter.Red); helper()
    }
  
  helper()


def get_valid_coordinate(state: GameState) = 
  get_valid_input(message = "Enter a tile position:",
                  parse_and_validate = input => parse_coordinate(input).flatMap(x => valid_coordinate(state, x)))


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
def get_valid_action(state: GameState)(pos: Coordinate): PlayerAction = 
  get_valid_input(message = "Enter an action: R for reveal, F for flag, U for unflag",
                  parse_and_validate = input => parse_action(input, pos).flatMap(action => valid_action(state, action)))


def parse_action(input: String, pos: Coordinate): Either[String, PlayerAction] = 
  input match {
    case "R" => Right(PlayerAction(Action.Reveal, pos))
    case "F" => Right(PlayerAction(Action.Flag, pos))
    case "U" => Right(PlayerAction(Action.Unflag, pos))
    case _ => Left("Please only enter R, F, or U: ")
  }


def valid_action(state: GameState, player_action: PlayerAction): Either[String, PlayerAction] = 
  val playertile = state.player_board.tile_map(player_action.pos)

  player_action.action match {
    case Action.Flag => {
      if playertile == PlayerTile.Hidden then 
        Right(player_action) 
      else 
        Left("You cannot flag a tile that's already revealed or flagged.\n")
      }
    case Action.Reveal => {
      if playertile == PlayerTile.Hidden then 
        Right(player_action) 
      else 
        Left("You cannot reveal a tile that's already revealed or flagged.\n")
      }
    case Action.Unflag => {
      if playertile == PlayerTile.Flagged then 
        Right(player_action) 
      else 
        Left("You cannot unflag a tile that's not flagged.\n")
    }
  }



//////////////
// Printers //
//////////////

// Clear the screen and move the cursor to the top-left corner
def print_inplace(): Unit = 
  print("\u001b[2J")
  print("\u001b[H")


// TODO: Print board underneath board size and number of mines info
def print_board[T](board: Board[T]): Unit = 
  print_inplace()
  val str_board = Array.fill(board.xsize)(Array.fill(board.ysize)(""))
  board.tile_map.map((tile_pos, tile) => str_board(tile_pos._1)(tile_pos._2) = tile.toString())
  print_helper[String](str_board)


// * Prints boards in the matrix form (Array of Arrays)
// ** You give
// board : Array of Arrays of type T, where T can be Int for input board or String for Tile
// ** You get
// print out of the board  
def print_helper[T](board: Array[Array[T]]): Unit = 
    println(board.map(_.mkString("")).mkString("\n"))
    println("\n")


// Rust style Result type
// enum ValidationResult:
//   case Ok(result: PlayerAction)
//   case Error(message: String)

// enum Result[T,E]:
//   case Ok(result: T)
//   case Error(message: E)
