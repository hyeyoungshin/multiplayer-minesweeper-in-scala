import scala.io.StdIn.readLine

object ColorPrinter:
  val Reset = "\u001b[0m"
  val Red = "\u001b[31m"
  val Blue = "\u001b[34m"
  val Green = "\u001b[32m"

  def print_in_color(text: String, color: String): Unit = {
    println(s"$color$text$Reset")
  }

  // def print_red(text: String): Unit = {
  //   println(s"$Red$text$Reset")
  // }

  // def print_blue(text: String): Unit = {
  //  println(s"$Blue$text$Reset")
  // }

  // def print_green(text: String): Unit = {
  //  println(s"$Green$text$Reset")
  // }

@main def text_ui_game(): Unit = 
  print_start()
  // Not using make_get_valid_input because it does not require `state` and `validate`
  val difficulty = get_valid_difficulty()
  print_difficulty(difficulty)

  var state = new_game(difficulty)
  print_state(state)

  while !game_over(state) do 
    val input_coordinate = get_valid_coordinate(state)
    val coordinate = convert_input_coordinate(input_coordinate)
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
  
  parsed  match {
    case Some(difficulty) => difficulty
    case None => get_valid_difficulty()
  }

def parse_difficulty(user_input: String): Option[GameDifficulty] = 
  user_input match {
    case "Easy" => Some(Easy)
    case "Intermediate" => Some(Intermediate)
    case "Hard" => Some(Hard)
    case _ => None
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
def make_get_valid_input[T](message: String, parse: String => Option[T], validate: (GameState, T) => Boolean)
: GameState => T = 
  def get_valid_input(state: GameState): T = 
    ColorPrinter.print_in_color(message, ColorPrinter.Blue)
    val player_input = readLine()
    val parsed_and_validated = parse(player_input).filter(x => validate(state, x))
    parsed_and_validated match {
      case Some(value) => value
      case None => get_valid_input(state)
    }
  
  get_valid_input

val get_valid_coordinate = make_get_valid_input(message = "Enter a tile position:",
                                                parse = parse_coordinate,
                                                validate = valid_coordinate)

/* parse user input via int array
for example, 1,1 is [1,1]
int array of length other than 2 is considered invalid
*/ 
def parse_coordinate(user_input: String): Option[InputCoordinate] =
  val option_parsed = parse_coordinate_helper(user_input)
  option_parsed.filter(arr => arr.length == 2) match {
    case Some(arr) => Some(InputCoordinate(arr(0), arr(1)))
    case None => None
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
def valid_coordinate(state: GameState, user_input: InputCoordinate): Boolean = 
  val tile_pos = convert_input_coordinate(user_input)
  judge_coordinate(state, tile_pos) match {
    case Left(bool) => bool && state.player_board.within_boundary(tile_pos)
    case Right(str) => ColorPrinter.print_in_color(str, ColorPrinter.Red); false
  }

def judge_coordinate(state: GameState, pos: Coordinate): Either[Boolean, String] = 
  state.player_board.tile_map(pos) match {
    case PlayerTile.Revealed(_) => Right("The tile is alreay revealed.\n")
    case _ => Left(true)
  }


///////////////////
// Player Action //
///////////////////
def get_valid_action(state: GameState, pos: Coordinate): PlayerAction = 
  ColorPrinter.print_in_color("Enter an action: R for reveal, F for flag, U for unflag", ColorPrinter.Blue)
  val input = readLine()
  val parsed_and_validated = parse_action(input, pos).filter(x => valid_action(state, x))
  
  parsed_and_validated match {
    case Some(player_action) => player_action
    case None => get_valid_action(state, pos)
  }

def parse_action(input: String, pos: Coordinate): Option[PlayerAction] = 
  input match {
    case "R" => Some(PlayerAction.Reveal(pos))
    case "F" => Some(PlayerAction.Flag(pos))
    case "U" => Some(PlayerAction.Unflag(pos))
    case _ => None
  }

def valid_action(state: GameState, action: PlayerAction): Boolean = 
  val playertile = state.player_board.tile_map(action.get_pos())
  judge_action(action, playertile) match {
    case Left(bool) => bool
    case Right(str) => ColorPrinter.print_in_color(str, ColorPrinter.Red); false // is there another way to do this?
  }

def judge_action(action: PlayerAction, playertile: PlayerTile): Either[Boolean, String] = 
  action match {
    case PlayerAction.Flag(_) => {
      if playertile == PlayerTile.Hidden then 
        Left(true) 
      else 
        Right("You cannot flag a tile that's already revealed or flagged.\n")
      }
    case PlayerAction.Reveal(_) => {
      if playertile == PlayerTile.Hidden then 
        Left(true) 
      else 
        Right("You cannot reveal a tile that's already revealed or flagged.\n")
      }
    case PlayerAction.Unflag(_) => {
      if playertile == PlayerTile.Flagged then 
        Left(true) 
      else 
        Right("You cannot unflag a tile that's not flagged.\n")
    }
  }
  

// Rust style Result type
// enum ValidationResult:
//   case Ok(result: PlayerAction)
//   case Error(message: String)

// enum Result[T,E]:
//   case Ok(result: T)
//   case Error(message: E)
