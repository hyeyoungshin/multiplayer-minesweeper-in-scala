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


case class Player(val id: PlayerID, val board: PlayerBoard)

type PlayerID = Int

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
            

// TODO: use map and range or list comprehension
def initialize_players(num_players: Int, player_board: PlayerBoard): PlayerPool = 
  def loop(counter: Int, acc: List[Player]): List[Player] = 
    if counter == 0 then
      acc
    else 
      loop(counter - 1, Player(id = counter,
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
    // play is only valid in GameStatus.Continue
    case GameStatus.Continue => {
      val updated_player_pool = (player_action.action, player_action.pos) match {
        case (Action.Reveal, pos) => update_player(state.player_pool, reveal(state.solution.board, pos))
        case (Action.Flag, pos) => update_player_pool(state.player_pool, flag(current_player, pos))
          // {val updated_players = state.player_pool.players.map(player => update_player(state, pos, flag(current_player)))
          // updated_players.foldRight(state.player_pool)((player, pool) => update_player_pool(pool, player))}
        case (Action.Unflag, pos) => update_player_pool(state.player_pool, unflag(current_player, pos))
      }
      
      update_state(state, updated_player_pool, player_action.pos)
    }

    case _ => throw IllegalStateException("You can only play game in Continue Status.")
  }

// update player with new player board in the current* pool
def update_player(player_pool: PlayerPool, update: PlayerBoard => PlayerBoard): PlayerPool = {
  val original_player_list = player_pool.players
  val current = player_pool.next
  val current_player = original_player_list(current)
  
  PlayerPool(original_player_list.updated(current, Player(current_player.id, update(current_player.board))), current)
}

// pos is not manipulated in the body, it's just passed along to action, so remove it
def update_player_pool(player_pool: PlayerPool, update: PlayerBoard => Option[PlayerBoard])
: PlayerPool = {
  val updated_players = player_pool.players.map(player => update(player.board) match {
    case Some(new_player_board) => Player(player.id, new_player_board)
    case None => Player(player.id, player.board) // tile at pos was already revealed or flagged // TODO
  })

  PlayerPool(players = updated_players, next = player_pool.next)
}


// def update_player(state: GameState, pos: Coordinate, update: ((PlayerBoard, Coordinate) => PlayerBoard) | 
//                                                              ((PlayerBoard, Coordinate) => Option[PlayerBoard])
//   ): Player = {
//   val current_player = state.player_pool.current()
  
//   update(current_player.board, pos) match {
//     case player_board: PlayerBoard => Player(current_player.id, player_board)
//     case option_player_board: Option[PlayerBoard] => option_player_board match {
//       case Some(player_board) => Player(current_player.id, player_board)
//       case None => update_player(state, get_valid_coordinate(state), update)
//     }
//   }
// }


// def update_player_pool(player_pool: PlayerPool, new_player: Player): PlayerPool = 
//   PlayerPool(player_pool.players.updated(player_pool.next, new_player), player_pool.next)                                         


def update_state(state: GameState, new_player_pool: PlayerPool, tile_pos: Coordinate): GameState = {
  val new_player_board = new_player_pool.current().board
  // update status
  val new_status = 
    if has_won(state.solution.board, new_player_board) then
      GameStatus.Win
    else
      new_player_board.tile_map(tile_pos) match {
        case PlayerTile.Revealed(SolutionTile.Mine) => GameStatus.Lose
        case PlayerTile.Revealed(SolutionTile.Empty) => GameStatus.Continue

        case PlayerTile.Revealed(SolutionTile.Hint(_)) => GameStatus.Continue
        case PlayerTile.Flagged(_) => GameStatus.Continue
        case PlayerTile.Hidden => GameStatus.Continue
      }
    
  GameState(state.solution, new_player_pool, new_status)
}


def next_player(state: GameState): GameState = {
  val current_pool = state.player_pool
  //TODO: make it PlayerPool method
  val next_pool = PlayerPool(current_pool.players, (current_pool.next + 1) % current_pool.players.length)
  
  // GameState(state.solution, next_pool, state.status)
  // copy makes it easy if additional fields are added later
  state.copy(player_pool = next_pool)
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
