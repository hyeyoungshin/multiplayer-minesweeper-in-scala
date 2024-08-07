import scala.io.StdIn.readLine
import java.awt.Color
import upickle.implicits.Readers

///////////////////////////////////////////////////////////////
//////////// Text based Single Player Minesweeper /////////////
///////////////////////////////////////////////////////////////
@main def text_ui_game(): Unit = 
  print_start()
  
  val difficulty = get_valid_difficulty()
  print_difficulty(difficulty)

  var state = new_game(difficulty)
  print_state(state)

  while !game_over(state) do 
    print_state(state)
    val player_action = get_valid_inputs(state)
    // val coordinate = get_valid_coordinate(state)
    // val player_action = get_valid_action(state)(coordinate)
    state = play(state, player_action)
    print_state(state)
    state = next_player(state)
    


//////////////////////////////////
////// Game Start and Setup //////
//////////////////////////////////  
def print_start(): Unit = {
  println("")
  print_with_effect("Welcome to the minesweeper game.", PrinterEffects.Bold)
}


//////////////////////////////
// Parse and Validate Input //
//////////////////////////////
/* Used to parse and validate user input for T which is one of
 - GameDifficulty   
 - Coordinate
 - PlayerAction
*/
def get_valid_input[T](message: String, parse_and_validate: String => Either[String, T]): T = {
  def loop(): T = 
    print_with_effect(message, PrinterEffects.Bold)
    val player_input = readLine()
    parse_and_validate(player_input) match {
      case Right(value) => value
      case Left(msg) => print_with_effect(msg, PrinterEffects.Red); loop()
    }
  
  loop()
}

// 
def get_valid_inputs(state: GameState): PlayerAction = {
  def loop(): PlayerAction = 
    val coordinate = get_valid_coordinate(state)
    val action = get_valid_action()
    valid_player_action(state, PlayerAction(action, coordinate)) match {
      case Right(action) => action
      case Left(msg) => print_with_effect(msg, PrinterEffects.Bold); loop()
    }

  loop()
}
  // get_valid_action(state)(get_valid_coordinate(state))


/////////////////////
// Game Difficulty //
/////////////////////  
def get_valid_difficulty(): GameDifficulty = {
  get_valid_input("Choose difficulty: (Easy, Intermediate, Hard)", parse_difficulty)
}


def parse_difficulty(user_input: String): Either[String, GameDifficulty] = {
  user_input match {
    case "Easy" => Right(Easy)
    case "Intermediate" => Right(Medium)
    case "Hard" => Right(Hard)
    case _ => Left("Enter Easy, Intermediate, or Hard only.")
  }
}


////////////////
// Coordinate //
////////////////
def get_valid_coordinate(state: GameState): Coordinate = {
  val current_player = state.player_pool.current()

  get_valid_input(message = s"Player ${current_player.id.n}, enter a tile position:",
                  parse_and_validate = input => parse_coordinate(input).flatMap(x => valid_coordinate(state, x)))
}
    

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
  val current_player = state.player_pool.current()
  if current_player.board.within_boundary(tile_pos) then
    current_player.board.tile_map(tile_pos) match {
      case PlayerTile(Some(s_tile), _) => Left("The tile is alreay revealed.")
      case _ => Right(tile_pos)
    }
  else 
    Left("The tile position is outside the boundary.")



///////////////////
// Player Action //
///////////////////
def get_valid_action(): Action = 
  get_valid_input(message = "Enter an action: R for reveal, F for flag, U for unflag",
                  parse_and_validate = input => parse_action(input))
                  

def parse_action(input: String): Either[String, Action] = 
  input match {
    case "R" => Right(Action.Reveal)
    case "F" => Right(Action.Flag)
    case "U" => Right(Action.Unflag)
    case _ => Left("Please only enter R, F, or U: ")
  }


def valid_player_action(state: GameState, player_action: PlayerAction): Either[String, PlayerAction] = {
  val current_player = state.player_pool.current()
  val p_tile = current_player.board.tile_map(player_action.pos)
  

  player_action.action match {
    case Action.Flag => p_tile match {
      case PlayerTile(None, None) => Right(player_action) 
      case PlayerTile(None, Some(player_id)) => Left(s"The tile is already flagged by Player ${player_id}.")
      case PlayerTile(Some(tile), _) => Left(s"The tile is already revealed.")
      
      // case PlayerTile.Hidden => Right(player_action) 
      // case PlayerTile.Flagged(by) => Left(s"The tile is already flagged by Player ${by.id}.")
      // case PlayerTile.Revealed(tile) => Left(s"The tile is already revealed.")
    }
    
    case Action.Reveal => p_tile match {
      case PlayerTile(None, None) => Right(player_action) 
      case PlayerTile(None, Some(player_id)) => 
        if player_id != current_player.id then 
          Right(player_action) // you can reveal a tile flagged by another player
        else
          Left(s"The tile is already flagged by Player ${player_id}")
      case PlayerTile(Some(tile), _) => Left(s"The tile is already revealed.")
    }
    
    case Action.Unflag => p_tile match {
        case PlayerTile(None, Some(player_id)) =>  
          if player_id == current_player.id then 
            Right(player_action)
          else
            Left(s"The tile is flagged by Player ${player_id}.")
        case _ => Left("You cannot unflag a tile that's not flagged.") 
      }
    }
}



//////////////
// Printers //
//////////////
object PrinterEffects:
  val Red = "\u001b[31m"
  val Blue = "\u001B[34m"
  val Bold = "\u001b[1m"
  val Reset = "\u001b[0m"


def print_with_effect(text: String, effect: String): Unit = {
  println(s"$effect$text${PrinterEffects.Reset}")
}


def print_inplace(): Unit = 
  // Clears the screen
  print("\u001b[2J")
  // Moves the cursor to the top-left corner
  print("\u001b[H")


def print_difficulty(difficulty: GameDifficulty): Unit = {
  println("")
  print_with_effect(s"Starting a game with board size: (${difficulty.size._1} x ${difficulty.size._2}) " +
    s"and Number of mines: ${difficulty.num_mines}", PrinterEffects.Bold)
  println("")
 
  Thread.sleep(1500)
}


def player_tile_to_string(p_tile: PlayerTile): String = p_tile match {
  case PlayerTile(None, None) => "[ ]"
  case PlayerTile(None, Some(player_id)) => "[F]" //TODO: add color by player
  case PlayerTile(Some(s_tile), _) => solution_tile_to_string(s_tile)
}


def solution_tile_to_string(s_tile: SolutionTile): String = s_tile match {
  case SolutionTile.Empty => "[E]"
  case SolutionTile.Mine => "[x]"
  case SolutionTile.Hint(n) => s"[$n]"

}


def print_board(player_board: PlayerBoard): Unit = 
  val str_board = Array.fill(player_board.xsize)(Array.fill(player_board.ysize)(""))
  player_board.tile_map.map((tile_pos, tile) => str_board(tile_pos._1)(tile_pos._2) = player_tile_to_string(tile))
  print_helper[String](str_board)


// * Prints boards in the matrix form (Array of Arrays)
// ** You give
// board : Array of Arrays of type T, where T can be Int for input board or String for Tile
// ** You get
// print out of the board  
def print_helper[T](board: Array[Array[T]]): Unit = 
  println(board.map(_.mkString("")).mkString("\n"))
  println("")


def print_state(state: GameState): Unit = 
  print_inplace()
  val current_player = state.player_pool.current()
  print_with_effect(s"Board: Player ${current_player.id.n}", PrinterEffects.Bold)
  print_with_effect(s"Number of Mines: ${state.solution.num_mines}", PrinterEffects.Bold)

  print_board(state.player_pool.current().board)
  Thread.sleep(500)

  state.status match {
    case GameStatus.Win(player) => print_with_effect(s"Player ${player.id.n} win!", PrinterEffects.Bold)
    case GameStatus.Lose => print_with_effect(s"Player ${current_player.id.n} lost...", PrinterEffects.Bold)
    case GameStatus.Continue => ()
  }


// Rust style Result type
// enum ValidationResult:
//   case Ok(result: PlayerAction)
//   case Error(message: String)

// enum Result[T,E]:
//   case Ok(result: T)
//   case Error(message: E)
