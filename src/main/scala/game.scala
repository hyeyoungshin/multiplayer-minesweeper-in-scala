////////////////////////////
///// Data types: Game /////
////////////////////////////

case class GameState (solution: Solution,         
                      playerstate: PlayerState, // players: List[Player], playerboards: List[PlayerBoards], next: Int
                      status: GameStatus)           

case class Solution(board: SolutionBoard, num_mines: Int)

enum GameStatus:
  case Win(winners: Set[PlayerID]) // Note: List vs. Set  
  case Continue

case class GameDifficulty(board_size: (Int, Int), num_mines: Int)

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
// class PlayerPool(val current_player_states: List[PlayerState], val next: Int) {
//   // Returns the current PlayerState
//   def current(): PlayerState = current_player_states(next)
  
//   // Updates the player pool by incrementing the index 
//   def next_pool(): PlayerPool = PlayerPool(current_player_states, (next + 1) % current_player_states.length)

//   // Returns the players except the current PlayerState
//   // Useful for getting the winners if the current player lost
//   def get_rest(): Set[PlayerState] = current_player_states.toSet.-(this.current())
// }

case class PlayerState(players: List[Player], boards: List[PlayerBoard], current: Int) {
  // Returns the rest of the players' ids
  // Useful for getting the winners when current player lost the game  
  def get_the_others(): Set[PlayerID] = players.map(player => player.id).toSet.-(players(current).id)
  
  // Updates playerstate for next player by incrementing the index 
  def next(): PlayerState = this.copy(current = (current + 1) % players.length)
}



// Contains information about each player
//
// * Input
// id : player id
// board : player's current board
// color: player's flag color
case class Player(id: PlayerID, color: PlayerColor)

case class PlayerID(n: Int)


// To distinguish each player's flags
case class PlayerColor(code: String)

final val Red =  PlayerColor("\u001b[31m")
final val Blue = PlayerColor("\u001B[34m")

// Gets all player colors
def player_colors(): List[PlayerColor] = List(Red, Blue)


// Provides a way to check validity of an action in combination with the coordinate
// * Input
// action : one of Reveal, Flag, and Unflag
// pos : the tile coordinate
case class PlayerAction(action: Action, tile_pos: Coordinate)

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
  val playerstate = initialize_playerstate(2, player_board)    
  
  GameState(Solution(solution_board, difficulty.num_mines), 
            playerstate,
            GameStatus.Continue)
            

// Initializes playerstate for the new game
// Each player is assigned an id, color, and board
def initialize_playerstate(num_players: Int, playerboard: PlayerBoard): PlayerState = {
  val players = (0 until num_players).map(n => Player(id = PlayerID(n), 
                                                      color = player_colors()(n))).toList

  val playerboards = players.map(player => playerboard)
      
  PlayerState(players, playerboards, 0)
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
  val playerstate = state.playerstate
  val current = playerstate.current
  val current_player_id = playerstate.players(current).id
  
  state.status match {
    // play is only valid in GameStatus.Continue
    case GameStatus.Continue => {
      val updated_playerstate = player_action.action match {
        case Action.Reveal => update_player(state.playerstate, reveal(state.solution.board, player_action.tile_pos))
        case Action.Flag => update_playerstate(state.playerstate, flag(current_player_id, player_action.tile_pos))
        case Action.Unflag => update_playerstate(state.playerstate, unflag(current_player_id, player_action.tile_pos))
      }
      val updated_status = update_status(updated_playerstate, player_action.tile_pos, state.solution.board)

      state.copy(playerstate = updated_playerstate, status = updated_status)
    }

    case _ => throw IllegalStateException("You can only play game in Continue Status.")
  }

// Updates the player in the current pool
// Mainly for when the player takes the reveal action which only affects their own board
//
// * Input
// playerpool: current player pool
// update: a function that updates the current player's board; most likely to be `reveal`
// * Output
// A new playerpool with only the current player's board updated; next index unchanged
def update_player(playerstate: PlayerState, update: PlayerBoard => PlayerBoard): PlayerState = {
  val current = playerstate.current

  playerstate.copy(boards = playerstate.boards.updated(current, update(playerstate.boards(current))))
}


// Updates every player's board in the playerpool with the update function
// 
// * Input
// playerpool: current playerpool
// update: either flag or unflag
// * Output
// A new playerpool where all players' boards are updated; next index unchanged
def update_playerstate(playerstate: PlayerState, update: PlayerBoard => Option[PlayerBoard]): PlayerState = {
  val updated_boards = playerstate.boards.map(board => 
    update(board) match {
      case Some(updated_board) => updated_board
      case None => board
    }
  )

  playerstate.copy(boards = updated_boards)
}


// Computes the new game status by checking if the current player has won or lost
// - If the current player revealed a mine, the rest of the players win
// - If the current player's board satisfies the winning conditon, winners are computed
// - Otherwise, game continues
def update_status(playerstate: PlayerState, tile_pos: Coordinate, solution_board: SolutionBoard): GameStatus = {
  val current_players_board = playerstate.boards(playerstate.current)
  
  if revealed_mine(current_players_board, tile_pos) then
    GameStatus.Win(playerstate.get_the_others())
  else if all_mines_flagged(current_players_board, solution_board) then
    GameStatus.Win(who_won(playerstate, solution_board))
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
def who_won(playerstate: PlayerState, solution_board: SolutionBoard): Set[PlayerID] = {
  // 1. Whoever has flagged most mines win
  // val num_flagged_mines_per_player = playerpool.players.map(
  //   player => num_flagged_mines_by(player, mine_coordinates(solution_board)))
  // val max_num_flags = num_flagged_mines_per_player.max
  // val winners_indices = num_flagged_mines_per_player.zipWithIndex.flatMap(
  //   (num_flags, i) => if num_flags == max_num_flags then Some(i) else None)
  
  // winners_indices.map(i => playerpool.players(i)).toSet

  // 2. Whoever has max points win (flag on mine + 1; flag on non-mine - 1)
  // val points_per_player = playerstate.boards.zipWithIndex.map(playerboard_i=> get_points(playerboard_i, mine_coordinates(solution_board)))  
  val points_per_player = playerstate.boards.zipWithIndex.map((board, id) => get_points(PlayerID(id))(board, mine_coordinates(solution_board))
  )
  val max_points = points_per_player.max
  val winners_indices = points_per_player.zipWithIndex.flatMap((points, i) => if points == max_points then Some(i) else None)
  
  winners_indices.map(i => playerstate.players(i).id).toSet
}

// Computes how many flags are on mines by the player
// def num_flagged_mines_by(player: Player, mine_coordinates: Set[Coordinate]): (Int, Int) = {
//   val num_flagged_mines = mine_coordinates.foldRight(0)((pos, acc) => player.board.tile_map(pos) match {
//     case PlayerTile.Flagged(flagger) if player.id == flagger.id => acc + 1
//     case _ => acc
//   })

//   val num_all_flags = player.board.tile_map.foldRight(0)((x, acc) => x._2 match {
//     case PlayerTile.Flagged(flagger) if player.id == flagger.id => acc + 1
//     case _ => acc
//   })

//   val num_flagged_non_mines = num_all_flags - num_flagged_mines

//   (num_flagged_mines, num_flagged_non_mines)
// }


// Calculates points for the player
def get_points(id: PlayerID)(board: PlayerBoard, mine_coordinates: Set[Coordinate]): Int = {
  val num_all_flags = board.tile_map.foldRight(0)((x, acc) => x._2 match {
    case PlayerTile.Flagged(flagger) if id == flagger => acc + 1
    case _ => acc
  })

  // println(s"num_all_flags is: ${num_all_flags}")

  val num_flagged_mines = mine_coordinates.foldRight(0)((pos, acc) => board.tile_map(pos) match {
    case PlayerTile.Flagged(flagger) if id == flagger => acc + 1
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
def next_player(state: GameState): GameState = state.copy(playerstate = state.playerstate.next())



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
def num_all_flags_by(player: Player): Int = {
  player.board.tile_map.foldRight(0)((x, z) => x._2 match {
    case PlayerTile.Flagged(flagger) if player.id == flagger.id => z + 1
    case _ => z
  })
}


/* 
`for_mine` = true :  calculates the number of flagged mines -> + 1
`for_mine` = false : calcultaes the number of non-mine tiles that are flagged -> - 1
 */
// TODO: need better name or remove boolean flag arg
def num_flags_by(player: Player, for_mine: Boolean, solution_board: SolutionBoard): Int = {
  val s_tiles = // either mines or non-mines (hint, empty)
    if for_mine then
      solution_board.tile_map.filter((pos, s_tile) => s_tile == SolutionTile.Mine)
    else
      solution_board.tile_map.filter((pos, s_tile) => s_tile != SolutionTile.Mine)

  s_tiles.foldRight(0)((x, z) => player.board.tile_map(x._1) match {
    case PlayerTile.Flagged(flagger) if player.id == flagger.id => z + 1
    // case PlayerTile.RNF(_, by) if by == player.id => z + 1
    // case Some(id) if player.id == id => z + 1
    case _ => z
  })
}


// TODO: maybe I need to implement what == means for PlayerTile
def num_flags_by_tile(player: Player, player_tile: PlayerTile, solution_board: SolutionBoard): Int = {
  player.board.tile_map.foldRight(0)((x, z) => if x._2 == player_tile then z + 1 else z)
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
