////////////////////////////
///// Data types: Game /////
////////////////////////////

case class GameState (val solution: Solution,         
                      val playerpool: PlayerPool,
                      val status: GameStatus)           

case class Solution(val board: SolutionBoard, val num_mines: Int)

enum GameStatus:
  case Win(winners: Set[Player]) // Note: List vs. Set  
  case Continue

case class GameDifficulty(val board_size: (Int, Int), val num_mines: Int)

final val Easy = GameDifficulty(board_size = (3, 3), num_mines = 2)
final val Medium = GameDifficulty(board_size = (5, 5), num_mines = 4)
final val Hard = GameDifficulty(board_size = (7, 7), num_mines = 9)


//////////////////////////////
///// Data types: Player /////
//////////////////////////////

// keeps track of the current player with the index `next`
// updates the current player's or everyone's board after each move 
//
// * Input
// players: the list of players in the game
// next: the index of the current player 
case class PlayerPool(playerstates: List[PlayerState], val current: Int) {
  def current_playerstate(): PlayerState = playerstates(current)

  def current_player(): Player = playerstates(current).player
  def current_playerboard(): PlayerBoard = playerstates(current).board
  
  // Updates the player pool by incrementing the index 
  def next_pool(): PlayerPool = this.copy(current = (current + 1) % playerstates.length)

  def get_players(): List[Player] = playerstates.map(playerstate => playerstate.player)

  // Returns the players except the current player
  // Useful for getting the winners if the current player lost
  def get_rest_players(): Set[Player] = {
    val the_others_states = playerstates.filter(playerstate => playerstate.player.id != current)
    the_others_states.map(the_other => the_other.player).toSet
  }
}

// Contains information about each player's current state
//
// * Input
// player
// board : player's current board
case class PlayerState(player: Player, board: PlayerBoard)


// Represents each player
//
// * Input
// id : an int value starting from 0
// color: the player's unique flag color
case class Player(id: Int, color: PlayerColor)


// To identify ownership of flags
case class PlayerColor(val code: String)

final val Red =  PlayerColor("\u001b[31m")
final val Blue = PlayerColor("\u001B[34m")

// Gets all player colors
def player_colors(): List[PlayerColor] = List(Red, Blue)


// Provides a way to check validity of an action in combination with the coordinate
// * Input
// action : one of Reveal, Flag, and Unflag
// pos : the tile coordinate
case class PlayerAction(val action: Action, val tile_pos: Coordinate)

enum Action:
  case Reveal, Flag, Unflag


//////////////////////////////
///////// Game Logic /////////
//////////////////////////////
def new_game(difficulty: GameDifficulty): GameState = 
  val mine_board = create_mineboard_for_game(difficulty)
  val solution_board = create_solutionboard(mine_board)
  val player_board = create_playerboard(mine_board.xsize, mine_board.ysize)

  // number of players is fixed as 2 now
  val playerpool = create_playerpool(2, player_board)    
  
  GameState(solution = Solution(solution_board, difficulty.num_mines), 
            playerpool = playerpool,
            status = GameStatus.Continue)
            

// Creates a new player pool for a new game
// Each player is given an id starting from 0, a color, and a new playerboard
def create_playerpool(num_players: Int, playerboard: PlayerBoard): PlayerPool = {
  val players = (0 until num_players).map(n => Player(id = n, color = player_colors()(n))).toList
  val playerstates = players.map(player => PlayerState(player, playerboard))

  PlayerPool(playerstates, 0)
}


// Checks whether the game should continue or end
def game_over(status: GameStatus): Boolean = 
  status match {
    case GameStatus.Win(_) => true
    case GameStatus.Continue => false
  }


// Updates the game state by applying changes to the playerpool and the game status base on the player's action 
// 1. playerpool
// - If Reveal: updates only the current player's board
// - If Flag, Unflag: updates all player's boards
// 2. game status (Win or Continue) 
// - If revealed a mine, game ends
// - If flagged all the mines, game ends
// - Otherwise, game continues
def play(state: GameState, player_action: PlayerAction): GameState = 
  val current_player = state.playerpool.current_player()
  
  state.status match {
    // play is only valid in GameStatus.Continue
    case GameStatus.Continue => {
      val updated_playerpool = player_action.action match {
        case Action.Reveal => update_player(state.playerpool, reveal(state.solution.board, player_action.tile_pos))
        case Action.Flag => update_playerpool(state.playerpool, flag(current_player, player_action.tile_pos))
        case Action.Unflag => update_playerpool(state.playerpool, unflag(current_player, player_action.tile_pos))
      }
      val updated_status = update_status(updated_playerpool, player_action.tile_pos, state.solution.board)

      state.copy(playerpool = updated_playerpool, status = updated_status)
    }

    case _ => throw IllegalStateException("You can only play game in Continue Status.")
  }

// Updates the current player's state in the current pool
// Mainly for when the player takes the reveal action which only affects their own board
//
// * Input
// playerpool: current player pool
// update: a function that updates the current player's board; most likely `reveal`
// * Output
// A new playerpool with only the current player's board updated
def update_player(playerpool: PlayerPool, update: PlayerBoard => PlayerBoard): PlayerPool = {
  val current = playerpool.current
  val current_playerstates = playerpool.playerstates
  val current_player = playerpool.current_player()
  val current_playerstate = playerpool.current_playerstate()
  val current_playerboard = playerpool.current_playerboard()
  
  // Return a copy of the payerpool where current player's board is updated by the `update` function
  playerpool.copy(playerstates = current_playerstates.updated(current, current_playerstate.copy(board = update(current_playerboard))))
}


// Updates every player's board in the current playerpool with the update function
// 
// * Input
// playerpool: current playerpool
// update: either flag or unflag
// * Output
// A new playerpool where all players' boards are updated; next index unchanged
def update_playerpool(playerpool: PlayerPool, update: PlayerBoard => Option[PlayerBoard]): PlayerPool = {
  val updated_playerstates = playerpool.playerstates.map(playerstate => 
    update(playerstate.board) match {
      case Some(updated_playerboard) => playerstate.copy(board = updated_playerboard)
      case None => playerstate
    }
  )

  playerpool.copy(playerstates = updated_playerstates)
}


// Computes the new game status by checking if the current player has won or lost
// - If the current player revealed a mine, the rest of the players win
// - If the current player's board satisfies the winning conditon, winners are computed
// - Otherwise, game continues
def update_status(playerpool: PlayerPool, tile_pos: Coordinate, solution_board: SolutionBoard): GameStatus = {
  val current_playerstate = playerpool.current_playerstate()
  
  if revealed_mine(current_playerstate.board, tile_pos) then
    GameStatus.Win(playerpool.get_rest_players())
  else if all_mines_flagged(current_playerstate.board, solution_board) then
    GameStatus.Win(who_won(playerpool, solution_board))
  else
    GameStatus.Continue
}


// Checks if a mine is revealed at the coordinate
def revealed_mine(playerboard: PlayerBoard, pos: Coordinate): Boolean = {
  playerboard.tile_map(pos) match {
    case PlayerTile.Revealed(SolutionTile.Mine) => true
    case PlayerTile.RevealedNFlagged(SolutionTile.Mine, _) => true
    case _ => false
  }
}



// Determine the winners
// The player(s) with the most number of flags on mines win (winning condition 1)
def who_won(playerpool: PlayerPool, solution_board: SolutionBoard): Set[Player] = {
  // 1. Whoever has flagged most mines win
  // val num_flagged_mines_per_player = playerpool.players.map(
  //   player => num_flagged_mines_by(player, mine_coordinates(solution_board)))
  // val max_num_flags = num_flagged_mines_per_player.max
  // val winners_indices = num_flagged_mines_per_player.zipWithIndex.flatMap(
  //   (num_flags, i) => if num_flags == max_num_flags then Some(i) else None)
  
  // winners_indices.map(i => playerpool.players(i)).toSet

  // 2. Whoever has max points win (flag on mine + 1; flag on non-mine - 1)
  val players = playerpool.get_players()

  val points_per_player = playerpool.playerstates.map(playerstate => get_points(playerstate, mine_coordinates(solution_board)))
  val max_points = points_per_player.max
  val winners_indices = points_per_player.zipWithIndex.flatMap((points, i) => if points == max_points then Some(i) else None)
  
  winners_indices.map(i => players(i)).toSet
}

// Computes how many flags are on mines by the player
def num_flagged_mines_by(playerstate: PlayerState, mine_coordinates: Set[Coordinate]): (Int, Int) = {
  val player = playerstate.player 
  val board = playerstate.board 

  val num_flagged_mines = mine_coordinates.foldRight(0)((pos, acc) => board.tile_map(pos) match {
    case PlayerTile.Flagged(flagger) if player == flagger => acc + 1
    case _ => acc
  })

  val num_all_flags = board.tile_map.foldRight(0)((x, acc) => x._2 match {
    case PlayerTile.Flagged(flagger) if player == flagger => acc + 1
    case _ => acc
  })

  val num_flagged_non_mines = num_all_flags - num_flagged_mines

  (num_flagged_mines, num_flagged_non_mines)
}


// Calculates points for the player
def get_points(playerstate: PlayerState, mine_coordinates: Set[Coordinate]): Int = {
  val player = playerstate.player 
  val board = playerstate.board 

  val num_all_flags = board.tile_map.foldRight(0)((x, acc) => x._2 match {
    case PlayerTile.Flagged(flagger) if player == flagger => acc + 1
    case _ => acc
  })

  // println(s"num_all_flags is: ${num_all_flags}")

  val num_flagged_mines = mine_coordinates.foldRight(0)((pos, acc) => board.tile_map(pos) match {
    case PlayerTile.Flagged(flagger) if player == flagger => acc + 1
    case _ => acc
  })

  // println(s"num_flagged_mines is: ${num_flagged_mines}")

  val num_flagged_non_mines = num_all_flags - num_flagged_mines

  // println(s"num_flagged_non_mines is: ${num_flagged_non_mines}")
  // println(s"points: ${num_flagged_mines - num_flagged_non_mines}")

  num_flagged_mines - num_flagged_non_mines
}


/* Returns the list of mine Coordinates. */
def mine_coordinates(solution_board: SolutionBoard): Set[Coordinate] = {
  solution_board.tile_map.filter((_, s_tile) => s_tile == SolutionTile.Mine).keys.toSet
  // solution_board.tile_map.flatMap(x => x._2 match {
  //     case SolutionTile.Mine => Some(x._1)
  //     case _ => None
  //   }
  // ).toSet

  // use flatMap here or flatten later
}


/* Checks whether all mines are flagged */
def all_mines_flagged(player_board: PlayerBoard, solution_board: SolutionBoard): Boolean = {
  val mines = mine_coordinates(solution_board)

  // assumption: set of flagged pos is the same across all playerboards 
  val all_flagged = player_board.tile_map.map(x => x._2 match {
      case PlayerTile.Flagged(_) => Some(x._1) // flagged by this player
      case PlayerTile.RevealedNFlagged(_, _) => Some(x._1)  // flagged by other players
       case _ => None
    }//match
  ).toSet.flatten//inner map ->  Set[Option[Coordinate]]

  mines subsetOf all_flagged
}


/* Counts how many flags are placed on non-mines by each player 
   Used to determine winner
*/
// def num_flagged_non_mines_per_player(playerpool: PlayerPool, solution_board: SolutionBoard): List[Int] = {
//   playerpool.players.map(
//       player => player.board.tile_map.foldRight(0)((x, z) => x._2 match {
//           case PlayerTile.Flagged(flagger) if player.id == flagger.id && !is_mine(x._1, solution_board)  => z + 1
//           case _ => z
//         } // match
//       ) // foldRight
//     ) // map
// }


def is_mine(pos: Coordinate, solution_board: SolutionBoard): Boolean = {
  mine_coordinates(solution_board).contains(pos) // Set[Option[Coordinate]]
}


// Updates the game state by setting the next player's index
def next_player(state: GameState): GameState = state.copy(playerpool = state.playerpool.next_pool())



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
// def all_mines_flagged_by(player: Player, solution_board: SolutionBoard): Boolean = {
//   // locate all the mines in solution board
//   val mines = solution_board.tile_map.filter((pos, s_tile) => s_tile == SolutionTile.Mine)
//   // all and only the mines are flagged by a player
//   mines.foldRight(true)((x, b) => player.board.tile_map(x._1) match {
//     case PlayerTile.Flagged(by) if by.id == player.id => true && b
//     case _ => false && b
//   })
// }

// Example:
// [   ][   ][x|F]
// [ F ][x|F][   ]
// [   ][   ][   ]
// Once all_mines_flagged
//    there are two ways to win
//    1. leave flags only on the mines => has_won_1
//    2. stop here, and compute points => has_won_2

//    all of them are player 0
//    one of flags on mine is by player 1
//    flag on non mine is by player 1
//    flag on non mine is by player 0





  // val all_flagged_pos_per_player = playerpool.players.map(player => 
  //     player.board.tile_map.map(x => x._2 match {
  //       case PlayerTile.Flagged(_) => Some(x._1)
  //       case PlayerTile.RNF(_, _) => Some(x._1)
  //        case _ => None
  //     }//match
  //   ).toSet//inner map ->  Set[Option[Coordinate]]
  // )        //outer map ->  List[Set[Option[Coordinate]]]
  
  // val all_mine_pos = solution_board.tile_map.map(x => x._2 match {
  //     case SolutionTile.Mine => Some(x._1)
  //     case _ => None
  //   }
  // ).toSet // Set[Option[Coordinate]]

  // val all_flagged_pos_per_player.map(set_of_flagged_pos => 
  //   if set_of_flagged_pos == all_mine_pos then
  //     1
  //   else 
  //     0
  //   )

  //   GameStatus.Continue

  // if all_flagged_pos == all_mine_pos then
  //   GameStatus.Win(who_won(playerpool))
  // elseflagger
  //   GameStatus.Continue



  // equality types:
  // reference equality
  // data equlity -> case class

  // note: think about how data might be ordered in a data structure in the language
  

  // if num_all_flags_by(player) == num_mines then
  //   GameStatus.Win(player)
  // else if player.board.tile_map.foldRight(0)((x, z) => if x._2 == PlayerTile.Hidden then z + 1 else z) == 0 then
  //   GameStatus.Continue // there still are tiles to reveal or flags to unflag
  // else if num_flags_by(player, true, solution_board) > num_mines / 2 then 
  //   GameStatus.Win(player)
  // else
  //   GameStatus.Lose(player)


// Precondition: all the mines are flagged by `player`
// then we stop and calculate points
// + 1 for flags on mines
// - 1 for flags on non-mines
// def has_won_2(solution_board: SolutionBoard, players: List[Player]): GameStatus = {
//   val points = players.map(player => 
//     num_flags_by(player, true, solution_board) - num_flags_by(player, false, solution_board))

//   GameStatus.Win(players(points.indexOf(points.max)))
// }


/* 
Calculates the number of all flags including flags placed by other players 
*/
// def num_all_flags_by(player: Player): Int = {
//   player.board.tile_map.foldRight(0)((x, z) => x._2 match {
//     case PlayerTile.Flagged(flagger) if player.id == flagger.id => z + 1
//     case _ => z
//   })
// }


/* 
`for_mine` = true :  calculates the number of flagged mines -> + 1
`for_mine` = false : calcultaes the number of non-mine tiles that are flagged -> - 1
 */
// TODO: need better name or remove boolean flag arg
// def num_flags_by(player: Player, for_mine: Boolean, solution_board: SolutionBoard): Int = {
//   val s_tiles = // either mines or non-mines (hint, empty)
//     if for_mine then
//       solution_board.tile_map.filter((pos, s_tile) => s_tile == SolutionTile.Mine)
//     else
//       solution_board.tile_map.filter((pos, s_tile) => s_tile != SolutionTile.Mine)

//   s_tiles.foldRight(0)((x, z) => player.board.tile_map(x._1) match {
//     case PlayerTile.Flagged(flagger) if player.id == flagger.id => z + 1
//     // case PlayerTile.RNF(_, by) if by == player.id => z + 1
//     // case Some(id) if player.id == id => z + 1
//     case _ => z
//   })
// }


// TODO: maybe I need to implement what == means for PlayerTile
// def num_flags_by_tile(player: Player, player_tile: PlayerTile, solution_board: SolutionBoard): Int = {
//   player.board.tile_map.foldRight(0)((x, z) => if x._2 == player_tile then z + 1 else z)
// }


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
