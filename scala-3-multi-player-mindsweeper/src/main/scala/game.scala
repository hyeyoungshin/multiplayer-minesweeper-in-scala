import scala.io.StdIn.readLine
import scala.util.Random

val BOARD_SIZE = (4, 4)
val NUM_MINES = 3

enum MS_GameState:
  case Win
  case Lose
  case Continue(/*num_flags: Int,*/ solution_board: SolutionBoard, player_board: PlayerBoard)

  def print_state: Unit = 
    this match {
      case Win => println("You win!")
      case Lose => println("You lost...")
      case Continue(solution_board: SolutionBoard, player_board: PlayerBoard) => println("Enter a tile position separated by a comma: ")
    }



def within_boundary(pos: InputCoordinate): Boolean = 
  pos.row > 0 && pos.column > 0 && pos.row <= BOARD_SIZE._1 && pos.column <= BOARD_SIZE._2


def valid_user_input(state: MS_GameState, user_input: String): Boolean = 
  val int_arr = user_input.split(",").map(_.toInt)
  val input_coordinate = parse_user_input(user_input)

  int_arr.length == 2 && within_boundary(input_coordinate) && fresh(state, convert_input_coordinate(input_coordinate))


def fresh(state: MS_GameState, pos: Coordinate): Boolean = 
  state match {
    case MS_GameState.Continue(_, player_board) => player_board.tile_map(pos) == PlayerTile.Hidden
    case _ => false
  }


def parse_user_input(user_input: String): InputCoordinate = 
  val int_arr = user_input.split(",").map(_.toInt)
  
  InputCoordinate(int_arr(0), int_arr(1))


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
  
  initial_board.print_board
  println("Enter a tile position separated by a comma: ")
  
  MS_GameState.Continue(/*num_flags,*/ solution_board, initial_board)


def game_over(state: MS_GameState): Boolean = 
  state match {
    case MS_GameState.Lose => true
    case MS_GameState.Win => true
    case MS_GameState.Continue(solution_board, player_board) => false
  }


def win(solution_board: SolutionBoard, player_board: PlayerBoard): Boolean = 
  val hidden_pos = player_board.tile_map.filter((pos, tile) => tile == PlayerTile.Hidden).keys
  hidden_pos.foldLeft(true)((acc, pos) => acc && solution_board.tile_map(pos) == SolutionTile.Mine)


def update_state(state: MS_GameState, solution_board: SolutionBoard, player_board: PlayerBoard, tile_pos: Coordinate): MS_GameState = 
  if win(solution_board, player_board) then
    MS_GameState.Win
  else
    player_board.tile_map(tile_pos) match {
      case PlayerTile.Revealed(SolutionTile.Mine) => MS_GameState.Lose
      case PlayerTile.Revealed(SolutionTile.Empty) => MS_GameState.Continue(solution_board, player_board)
      case PlayerTile.Revealed(SolutionTile.Hint(n)) => MS_GameState.Continue(solution_board, player_board)
      case _ => throw IllegalStateException()
    }

  
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
  state match {
      case MS_GameState.Continue(solution_board, player_board) => 
        val cur_playerboard = reveal(solution_board, player_board, tile_pos) 
        cur_playerboard.print_board
        update_state(state, solution_board, cur_playerboard, tile_pos)
      case _ => throw IllegalStateException()
    }


@main def game(): Unit = 

  print_start()

  var state = new_game()

  while !game_over(state) do 
    var user_input = readLine()
    
    while !valid_user_input(state, user_input) do
      println("Enter a valid tile position: ")
      user_input = readLine()

    val user_input_coordinate = parse_user_input(user_input)
    val tile_pos = convert_input_coordinate(user_input_coordinate)

    state = play(state, tile_pos)
    state.print_state

