////////////////////////////
///// Data types: Game /////
////////////////////////////
case class GameState (val solution: Solution,         
                      val player_pool: PlayerPool,
                      val status: GameStatus)           
                      
case class Solution(val board: SolutionBoard, val num_mines: Int)

enum GameStatus:
  case Win 
  case Lose 
  case Continue

case class GameDifficulty(val size: (Int, Int), val num_mines: Int)
final val Easy = GameDifficulty((3, 3), 2)
final val Medium = GameDifficulty((5, 5), 4)
final val Hard = GameDifficulty((7, 7), 9)


//////////////////////////////
///// Data types: Player /////
//////////////////////////////
case class PlayerPool(val players: List[Player], val next: Int):
  def current(): Player = players(next)


case class Player(val name: String, val board: PlayerBoard)
  

case class PlayerAction(val action: Action, val pos: Coordinate)

enum Action:
  case Reveal, Flag, Unflag


//////////////////////////////
///////// Game Logic /////////
//////////////////////////////
def new_game(difficulty: GameDifficulty): GameState = 
  val mine_board = generate_mine_locations(difficulty)
  val solution_board = create_solutionboard(mine_board)
  val initial_board = create_playerboard(mine_board.xsize, mine_board.ysize)

  // number of players is fixed for now
  val player_pool = initialize_players(2, initial_board)    
  
  GameState(solution = Solution(solution_board, difficulty.num_mines), 
            player_pool = player_pool,
            status = GameStatus.Continue)
            


def initialize_players(num_players: Int, player_board: PlayerBoard): PlayerPool = 
  def loop(counter: Int, acc: List[Player]): List[Player] = 
    if counter == 0 then
      acc
    else 
      loop(counter - 1, Player(name = s"Player ${counter}",
                               board = player_board) :: acc)

  PlayerPool(loop(num_players, List()), 0)


def game_over(state: GameState): Boolean = 
  state.status match {
    case GameStatus.Lose => true
    case GameStatus.Win => true
    case GameStatus.Continue => false
  }


def play(state: GameState, player_action: PlayerAction): GameState = 
  val current_player = state.player_pool.current()

  state.status match {
    // play is only valid in Continue status
    case GameStatus.Continue => {
      val new_player_board = (player_action.action, player_action.pos) match {
        case (Action.Reveal, pos) => Some(reveal(state.solution.board, current_player.board, pos))
        case (Action.Flag, pos) => flag(current_player.board, pos)
        case (Action.Unflag, pos) => unflag(current_player.board, pos)
      }

      new_player_board match {
        case Some(player_board) => update_state(state, player_board, player_action.pos)
        case None => ??? // get_valid_input()
      }
    }
    case _ => throw IllegalStateException("You can only play game in Continue Status.")
  }


def update_state(state: GameState, new_player_board: PlayerBoard, tile_pos: Coordinate): GameState = {
  // update player in the pool with new player board
  val player_pool_with_updated_player = update_player(state, new_player_board)
  // update status
  val new_status = 
    if has_won(state.solution.board, new_player_board) then
      GameStatus.Win
    else
      new_player_board.tile_map(tile_pos) match {
        case PlayerTile.Revealed(SolutionTile.Mine) => GameStatus.Lose
        case PlayerTile.Revealed(SolutionTile.Empty) => GameStatus.Continue

        case PlayerTile.Revealed(SolutionTile.Hint(_)) => GameStatus.Continue
        case PlayerTile.Flagged => GameStatus.Continue
        case PlayerTile.Hidden => GameStatus.Continue
      }
    
  GameState(state.solution, player_pool_with_updated_player, new_status)
}


// update player with new player board in the current* pool
def update_player(state: GameState, new_player_board: PlayerBoard): PlayerPool = {
  val original_player_list = state.player_pool.players
  val current = state.player_pool.next
  val current_player = original_player_list(current)
  
  PlayerPool(original_player_list.updated(current, Player(current_player.name, new_player_board)), current)
}


def next_player(state: GameState): GameState = {
  val current_pool = state.player_pool
  val next_pool = PlayerPool(current_pool.players, (current_pool.next + 1) % current_pool.players.length)
  
  GameState(state.solution, next_pool, state.status)
}


/* player wins if they find all mines which means
number of hidden playertiles equals number of mines AND
number of flagged playertiles is 0
*/
def has_won(solution_board: SolutionBoard, player_board: PlayerBoard): Boolean = {
  val num_mines = solution_board.tile_map.count((_, tile) => tile == SolutionTile.Mine)
  val num_hidden = player_board.tile_map.count((_, tile) => tile == PlayerTile.Hidden)
  val num_flagged = player_board.tile_map.count((_, tile) => tile == PlayerTile.Flagged)

  num_hidden == num_mines && num_flagged == 0
}


// Node:
// Separate Game(model) and text ui(view).
// Game - Coordinate || text ui - InputCoordinate
// To clean up, we rebuilt via Metals
// Had nested sbt files 


// // tiles and mines ratio
// enum Difficulty:
//   case Easy //  12.6%
//   case Intermediate // 18.1%
//   case Expert // 20.6%
