////////////////////////////
///// Data types: Game /////
////////////////////////////
case class GameState (val solution: Solution,         
                      val player_pool: PlayerPool,
                      val status: GameStatus)           

case class Solution(val board: SolutionBoard, val num_mines: Int)

enum GameStatus:
  case Win(who: Player)
  case Lose 
  case Continue

case class GameDifficulty(val size: (Int, Int), val num_mines: Int)

final val Easy = GameDifficulty((3, 3), 2)
final val Medium = GameDifficulty((5, 5), 4)
final val Hard = GameDifficulty((7, 7), 9)


//////////////////////////////
///// Data types: Player /////
//////////////////////////////

/* Datatype PlayerPool
1. Keeps track of the current player with index `next`
2. updates the current player's or everyone's board after each move 
Fields:
  `players` is the list of players in the game
  `next` is the index of the current player 
Methods:
  `current` gives the current player in the pool
  `next_pool` gives a player pool with the current players and `next` index incremented
 */
case class PlayerPool(val players: List[Player], val next: Int):
  def current(): Player = players(next)
  def next_pool(): PlayerPool = PlayerPool(players, (next + 1) % players.length)
  def get_player(id: PlayerID): Player = players(id.n)


/* Datatype Player
Provides a way to construct a player
Fields:
  `id` is an int value and plays a role of player identifieer
  `board` represents the state of the player's board
*/
case class Player(val id: PlayerID, val board: PlayerBoard, val color: PlayerColor)


case class PlayerID(val n: Int)

case class PlayerColor(val code: String)

final val Red =  PlayerColor("\u001b[31m")
final val Blue = PlayerColor("\u001B[34m")

def player_colors(): List[PlayerColor] = List(Red, Blue)

def colors(): List[PlayerColor] = List(Red, Blue)

/* Datatype PlayerAction
Provides a way to check validity of an action in combination with tile coordinate
Fields:
  `action` is one of Reveal, Flag, and Unflag
  `pos` is a tile position in the board
*/
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
            

/* initializes a player pool for the new game
   each player is given an id and a new player board with all tiles hidden
*/
def initialize_players(num_players: Int, player_board: PlayerBoard): PlayerPool = {
  val players = (0 until num_players).map(n => Player(id = PlayerID(n), board = player_board, color = player_colors()(n))).toList
  
  PlayerPool(players, 0)
}


def game_over(state: GameState): Boolean = 
  state.status match {
    case GameStatus.Lose => true
    case GameStatus.Win(_) => true
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
  
  PlayerPool(players = original_player_list.updated(current, current_player.copy(board = update(current_player.board))), 
             next = current)
}

// pos is not manipulated in the body, it's just passed along to action, so remove it
def update_player_pool(player_pool: PlayerPool, update: PlayerBoard => Option[PlayerBoard])
: PlayerPool = {
  val updated_players = player_pool.players.map(player => update(player.board) match {
    case Some(new_player_board) => player.copy(board = new_player_board) // Player(player.id, new_player_board)
    case None => player.copy()
  })

  PlayerPool(players = updated_players, next = player_pool.next)
}


def update_state(state: GameState, new_player_pool: PlayerPool, tile_pos: Coordinate): GameState = {
  val new_player_board = new_player_pool.current().board
  // update status
  val new_status = 
    if all_mines_flagged(state.solution.board, new_player_board) then
      // has_won_1: if only mines are flagged by the current player
      has_won_1(state.player_pool.current(), state.solution.num_mines) 
      // has_won_2: sum up points for flags on mine + 1 and non-mine - 1
      // TODO: What if it's a tie?
      // has_won_2(state.solution.board, state.player_pool.players)
    else
      new_player_board.tile_map(tile_pos) match {
        case PlayerTile.Revealed(SolutionTile.Mine) => GameStatus.Lose
        case PlayerTile.RNF(SolutionTile.Mine, _) => GameStatus.Lose
        // case PlayerTile(Some(SolutionTile.Mine), _) => GameStatus.Lose
        case _ => GameStatus.Continue
      }
    
  GameState(state.solution, new_player_pool, new_status)
}


def next_player(state: GameState): GameState = {
  val current_pool = state.player_pool
  // copy makes it easy if additional fields are added later
  state.copy(player_pool = current_pool.next_pool())
}


/* In general, player wins if they find all mines by placing flags and avoiding revealing mines

There could be two scenarios:
  1. wins flags are only placed on mines (if there are flags on non-mine tiles, you need to unflag them to win)
  2. wins as soon as all mines are flagged (flags can be placed on non-mine tiles but there is penalty for that)
     or all non-mine tiles revealed

SolutionBoard Ex: 
((0, 0) -> SolutionTile.Mine), ((0, 1) -> SolutionTile.Hint(2)), ((0, 2) -> SolutionTile.Empty) ...

PlayerBoard Ex:
(((0, 0) -> PlayerTile(Some(SolutionTile.Mine)), None), 
((0, 1) -> PlayerTile(None, Some(PlayerID(1))), 
((0, 2) -> PlayerTile(Some(SolutionTile.Empty)), None) ...)
*/
def all_mines_flagged(solution_board: SolutionBoard, player_board: PlayerBoard): Boolean = {
  // locate all the mines in solution board
  val mines = solution_board.tile_map.filter((pos, s_tile) => s_tile == SolutionTile.Mine)
  // all and only the mines are flagged by a player
  mines.foldRight(true)((x, b) => player_board.tile_map(x._1) match {
    case PlayerTile.Flagged(_) => true && b
    case PlayerTile.RNF(_, _) => true && b
    case _ => false && b
  })
}

def has_won_1(player: Player, num_mines: Int): GameStatus = {
  if num_all_flags(player) == num_mines then
    GameStatus.Win(player)
  else
    GameStatus.Continue
}


def has_won_2(solution_board: SolutionBoard, players: List[Player]): GameStatus = {
  val points = players.map(player => 
    num_flags_on_mines_by(true, solution_board, player) - num_flags_on_mines_by(false, solution_board, player))

  GameStatus.Win(players(points.indexOf(points.max)))
}


def num_all_flags(player: Player): Int = {
  player.board.tile_map.foldRight(0)((x, z) => x._2 match {
    case PlayerTile.Flagged(_) => z + 1
    case PlayerTile.RNF(_, _) => z + 1
    case _ => z
  })
}

// def num_flags_on_mines(solution_board: SolutionBoard, player: Player): Int = {
//   val mines = solution_board.tile_map.filter((pos, s_tile) => s_tile == SolutionTile.Mine)

//   mines.foldRight(0)((x, z) => player.board.tile_map(x._1).flagged_by match {
//     case Some(id) if player.id == id => z + 1
//     case _ => z
//   })
// }


def num_flags_on_mines_by(is_mine: Boolean, solution_board: SolutionBoard, player: Player): Int = {
  val s_tiles = // type of tiles we look for flags
    if is_mine then
      solution_board.tile_map.filter((pos, s_tile) => s_tile == SolutionTile.Mine)
    else
      solution_board.tile_map.filter((pos, s_tile) => s_tile != SolutionTile.Mine)

  s_tiles.foldRight(0)((x, z) => player.board.tile_map(x._1) match {
    case PlayerTile.Flagged(by) if by == player.id => z + 1
    case PlayerTile.RNF(_, by) if by == player.id => z + 1
    // case Some(id) if player.id == id => z + 1
    case _ => z
  })
}


// def num_flags_not_on_mines(solution_board: SolutionBoard, player: Player): Int = {
//   val non_mines = solution_board.tile_map.filter((pos, s_tile) => s_tile != SolutionTile.Mine)

//   non_mines.foldRight(0)((x, z) => player.board.tile_map(x._1).flagged_by match {
//     case Some(id) if player.id == id => z + 1
//     case _ => z
//   })
// }



// Note:
// Separate Game(model) and text ui(view).
// Game - Coordinate || text ui - InputCoordinate
// To clean up, we rebuilt via Metals
// Had nested sbt files 


// // tiles and mines ratio
// enum Difficulty:
//   case Easy //  12.6%
//   case Intermediate // 18.1%
//   case Expert // 20.6%
