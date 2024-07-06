import scala.io.StdIn.readLine

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
  println("Welcome to the minesweeper game. \n")
  println("Enter your name: ")
  val user_name = readLine()
  println(s"Hello, $user_name!\n")
  println("")


/////////////////////
// Game Difficulty //
/////////////////////  
def get_valid_difficulty(): GameDifficulty = 
  println("Choose difficulty: (Easy, Intermediate, Hard)")
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
  println(s"Starting a game with board size: (${difficulty.size._1} x ${difficulty.size._2}) " +
    s"and Number of mines: ${difficulty.num_mines}")
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
    println(message)
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
  //TODO: use filter
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
  state.player_board.within_boundary(tile_pos) && 
  ((state.player_board.tile_map(tile_pos) == PlayerTile.Hidden) || 
   (state.player_board.tile_map(tile_pos) == PlayerTile.Flagged))


///////////////////
// Player Action //
///////////////////
def get_valid_action(state: GameState, pos: Coordinate): PlayerAction = 
  println("Enter an action. R for reveal, F for flag, U for unflag: ")
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

  action match {
    case PlayerAction.Flag(_) => playertile == PlayerTile.Hidden
    case PlayerAction.Reveal(_) => playertile == PlayerTile.Hidden
    case PlayerAction.Unflag(_) => playertile == PlayerTile.Flagged
  }



// TODO: implement print_action
// Think about how to use Either[PlayerAction, String] for print_action
// if (playertile == PlayerTile.Hidden) then Left(action) else Right("error message")

// Rust style Result type
// enum ValidationResult:
//   case Ok(result: PlayerAction)
//   case Error(message: String)

// enum Result[T,E]:
//   case Ok(result: T)
//   case Error(message: E)

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
