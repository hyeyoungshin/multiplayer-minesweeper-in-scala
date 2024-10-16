package minesweeper.game

import scala.util.Random

/////////////////////////////
//// Data types for Board////
/////////////////////////////
// The internal representation of the board
// * Input
// xsize: the board's horizontal size
// ysize: the board's vertical size
// tile_map: a map from a coordinate on the board to a tile
// * Output
// One of mineboard, player board, or solution board depending on the tile types
case class Board[Tile] (val xsize: Int, val ysize: Int, val tile_map: Map[Coordinate, Tile]) {
  // Checks a coordinate is within board boundary
  // i.e.,
  // [][][]
  // [][][]
  // [][][]
  // Coordinate(3,1) is not withint the boundary of the above board
  // * Input
  // tile_pos: a coordinate to check
  // * Output
  // whether the coordinate is within the boundary or not
  def within_boundary(tile_pos: Coordinate): Boolean = 
    tile_pos.x > -1 && tile_pos.y > -1 && tile_pos.x < xsize && tile_pos.y < ysize

  // Identifies neighboring coordinates
  // i.e.,
  //    0 1 2
  // 0 [ ][][]
  // 1 [ ][][]
  // 2 [x][][]
  // the neighboring coordinates of marked tile are ((0,1),(1,1),(1,2))
  // * Input
  // board: one of solution board, player board, mineboard
  // tile_pos: a cooredinate on the board
  // * Output
  // List of neighboring coordinates
  def neighbors_inbounds(tile_pos: Coordinate): List[Coordinate] = {
    val all_neighbors = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).map(
        (i, j) => Coordinate(tile_pos.x + i, tile_pos.y + j))
    
    all_neighbors.filter(tile_pos => this.within_boundary(tile_pos))
  }
}

// The three types of the boards to make the game work
type SolutionBoard = Board[SolutionTile]
type PlayerBoard = Board[PlayerTile]
type MineBoard = Board[Boolean]


enum SolutionTile: 
  case Empty
  case Mine
  case Hint (num_of_neighboring_mines: Int) // number of mines in the neighboring tiles


enum PlayerTile:
  case Hidden
  case Revealed (tile: SolutionTile)
  case Flagged (flagger: Player)
  case RevealedNFlagged(tile: SolutionTile, flagger: Player)
                                                  


// Internal representation of tile positions for the boards
// i.e., a 3 x 3 board:
// [(0,0)][(1,0)][(2,0)]
// [(0,1)][(1,1)][(2,1)]
// [(0,2)][(1,2)][(2,2)]

// * Input
// x : starting from top left corner (0, 0) grow to the right 
// y : starting from top left corner (0, 0) grow downwards
// * Output
// A tile position on a board
case class Coordinate (val x: Int, val y: Int)


////////////////////////
//// Creating Board/////
////////////////////////

// Creates a mineboard based on difficulty
// - Easy: a 3x3 board with 2 mines
// - Intermediate: a 5x5 board with 4 mines
// - Expert: a 7x7 board with 9 mines
// and the mine locations are randomly selected
// * Input
// difficulty: one of easy, intermediate, expert
// * Output
// A mineboard where a boolean value indicates whether or not mine is located
def create_mineboard_for_game(difficulty: GameDifficulty): MineBoard = {
  val xsize = difficulty.board_size._1 // 3
  val ysize = difficulty.board_size._2 // 3

  val coordinate_keys = generate_coordinate_keys(xsize, ysize) // ((0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)

  // randomly shuffle coordinate_keys and pick some
  val mine_coordinates = Random.shuffle(coordinate_keys).take(difficulty.num_mines)
  
  // [ ][ ][]
  // [*][ ][]
  // [ ][*][]
  // val mine_coordinates = List(Coordinate(0, 1), Coordinate(1, 2)) // fixed for test
  
  val boolean_tile_map = coordinate_keys.map(coordinate => 
    if mine_coordinates.contains(coordinate) 
    then 
      (coordinate, true) // mine
    else 
      (coordinate, false) // not mine
  ).toMap

  Board(
    xsize,
    ysize,
    boolean_tile_map
  ) 
}

// Creates SolutionBoard from MineBoard at the start of game
// Derives Hint from the mine locations in MineBoard
//
// ** You give
// mineboard: mine locations
// ** You get
// SolutionBoard
def create_solutionboard(mineboard: MineBoard): SolutionBoard = {
  val coordinate_keys = generate_coordinate_keys(mineboard.xsize, mineboard.ysize) 
    
  Board(
    xsize = mineboard.xsize,
    ysize = mineboard.ysize,
    tile_map = coordinate_keys.map(coordinate => (coordinate, generate_solution_tile(mineboard, coordinate))).toMap
  )
}

// Creates a mineboard from the array of array representation of mine locations from GameInput or test input
// A mineboard is necessary for creating a solution board
//
// * Input
// mine_locations : array of arrays where 1 means there is a mine, can be obtained from GameInput
// * Output
// MineBoard where true means a mine; false no mine
def create_mineboard_for_test(mine_locations: Array[Array[Int]]): MineBoard = {
  val xlen = mine_locations.length
  val ylen = mine_locations.head.length

  val coordinate_keys = generate_coordinate_keys(xlen, ylen)
  
  // [[0, 0, 1],
  //  [0, 0, 0],
  //  [0, 0, 1]]
  // mine locations: (0,2),(2,2)
  // converts to: Coordinate(2,0), Coordinate(2,2)
  Board(
    xsize = xlen,
    ysize = ylen,
    tile_map = coordinate_keys.map(coordinate => (coordinate, mine_locations(coordinate.y)(coordinate.x) == 1 )).toMap
  )
}

// Creates the initial PlayerBoard at the start of a game
//
// ** You give
// xlen : number of tiles horizontally
// ylen : number of tiles vertically
// ** You get
// PlayerBoard where all tiles are hidden
def create_playerboard(xsize: Int, ysize: Int): PlayerBoard = {
  val xlen = xsize
  val ylen = ysize
  val coordinate_keys = generate_coordinate_keys(xlen, ylen)
  
  Board(
    xsize = xlen,
    ysize = ylen,
    tile_map = coordinate_keys.map(coordinate => (coordinate, PlayerTile.Hidden)).toMap
  )
}


///////////////////////////////////// 
//// Updating Board by revealing ////
/////////////////////////////////////

// Updates the playerboard by creating a new playerboard with a new tile at the tile position
def update_player_board(playerboard: PlayerBoard, tile_pos: Coordinate, new_tile: PlayerTile): PlayerBoard = {
  Board(xsize = playerboard.xsize, 
        ysize = playerboard.ysize, 
        tile_map = playerboard.tile_map + (tile_pos -> new_tile))  
}


// Reveals the tile on the playerboard at the coordinate
// - If the revealed tile is 0, it revealed all its neighboring tiles
// - If the revealed tile is a mine, it reveals all existing mines
def reveal(solution_board: SolutionBoard, tile_pos: Coordinate)(player_board: PlayerBoard): PlayerBoard = {
  val solution_tile = solution_board.tile_map(tile_pos)
  val updated_board = player_board.tile_map(tile_pos) match {
    case PlayerTile.Hidden => update_player_board(player_board, tile_pos, PlayerTile.Revealed(solution_tile))
    case PlayerTile.Flagged(by) => update_player_board(player_board, tile_pos, PlayerTile.RevealedNFlagged(solution_tile, by))
    case _ => player_board
    
  }
  
  solution_tile match {
    case SolutionTile.Empty => reveal_neighbors(solution_board, updated_board, tile_pos)
    case SolutionTile.Mine => updated_board //reveal_all_mines(solution_board, updated_board)
    case SolutionTile.Hint(_) => updated_board // no further action required
  }
}


// Reveals the neighboring tiles of the tile at the coordinate on the playerboard 
// - If a neighboring tile is already revealed, nothing happens
// - If not, reveals it
def reveal_neighbors(solution_board: SolutionBoard, player_board: PlayerBoard, tile_pos: Coordinate): PlayerBoard = {
  val neighbors = solution_board.neighbors_inbounds(tile_pos)

  neighbors.foldLeft(player_board)(
    (acc, tile_pos) => player_board.tile_map(tile_pos) match {
      case PlayerTile.Revealed(_) => acc
      case PlayerTile.RevealedNFlagged(_, _) => acc
      case _ => reveal(solution_board, tile_pos)(acc)
    }
  )
}



// Reveals all existing mines on the playerboard
def reveal_all_mines(solution_board: SolutionBoard, player_board: PlayerBoard): PlayerBoard = {
  val mine_locations = mine_coordinates(solution_board)
    // solutionboard.tile_map.filter((tile_pos, tile) => tile == SolutionTile.Mine).keys
  
  mine_locations.foldLeft(player_board)((acc, pos) => 
    player_board.tile_map(pos) match {
      case PlayerTile.Flagged(flagger) => update_player_board(acc, pos, PlayerTile.RevealedNFlagged(SolutionTile.Mine, flagger))
      case _ => update_player_board(acc, pos, PlayerTile.Revealed(SolutionTile.Mine))
    }
  )
}



// Flags the tile at the coordinate on the playerboard
//
// * Input
// by: the player who's flagging the tile
// pos: the coordinate of the tile to flag
// playerboard
// * Output
// Some board if the tile was hidden (the only valid case)
// None, otherwise
def flag(by: Player, pos: Coordinate)(playerboard: PlayerBoard): Option[PlayerBoard] = {
  playerboard.tile_map(pos) match {
    case PlayerTile.Hidden => Some(update_player_board(playerboard, pos, PlayerTile.Flagged(by)))
    case _ => None // should not reach this case
  }
}  


// Unflags the tile at the coordinate on the playerboard
//   
// * Input
// player: the player who's unflagging the tile
// pos: the coordinate of the tile to unflag
// playerboard
// * Output
// Some board if the tile was flagged by the player
// None, otherwise
def unflag(player: Player, pos: Coordinate)(playerboard: PlayerBoard): Option[PlayerBoard] = {
  playerboard.tile_map(pos) match {
    case PlayerTile.Flagged(by) if by.id == player.id => Some(update_player_board(playerboard, pos, PlayerTile.Hidden))
    case PlayerTile.RevealedNFlagged(s_tile, _) => Some(update_player_board(playerboard, pos, PlayerTile.Revealed(s_tile)))
    case _ => None
  }
}


///////////////
//// Helper////
///////////////

// Counts number of neighboring mines
// i.e., 
// [false][flase][true ]
// [false][false][false]
// [false][false][true ]
// the number of neighboring mines for above mineboard at coordinate (1,2) is 1
// * Input
// mineboard: a boolean board where mine coordinates are true
// tile_pos: coordinate
// * Ouput
// the number of nieghboring mines
def count_neighboring_mines(mineboard: MineBoard, tile_pos: Coordinate): Int = {
  val neighbors = mineboard.neighbors_inbounds(tile_pos)

  // for each neighboring tile, add 1 if the tile is true (there is a mine)
  neighbors.foldLeft(0)((acc, tile_pos) => if mineboard.tile_map(tile_pos) then acc + 1 else acc )
}


// Creates a solution tile at the coordinate based on the number of neighboring mines
// 
// * Input
// mineboard : a boolean board where a coordinate is true if there is a mine 
// tile_pos : a coordinate on the board
// * Output
// A solution tile for the coordinate
def generate_solution_tile(mineboard: MineBoard, tile_pos: Coordinate): SolutionTile = {
  val num_mines_in_neighbor = count_neighboring_mines(mineboard, tile_pos)
  
  if mineboard.tile_map(tile_pos) then
    SolutionTile.Mine
  else if num_mines_in_neighbor == 0 then
    SolutionTile.Empty
  else 
    SolutionTile.Hint(num_mines_in_neighbor)
}


// * Generates coordinate keys for creating boards
// * i.e., for a board of size 3 * 3, it generates ((0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)
//
// ** Input
// xlen : the board's horizontal size
// ylen : the board's vertical size
// ** Output
// A list of coordinates for the board
def generate_coordinate_keys (xlen: Int, ylen: Int) : List[Coordinate] = {
  (0 until xlen).flatMap(x => (0 until ylen).map( y => Coordinate(x, y))).toList
}


// TODO: add test runner with json files
// @main def hello(): Unit = 
  // simulate("src/test/board_tests/4-in.json")

// def simulate(filename: String): Unit = 
//   val game_input = parse_game_input(filename)
//   val mine_board = create_mineboard(game_input.board)  
//   val solution_board = create_solutionboard(mine_board)
//   println("solution_board:")
//   solution_board.print_board

//   val initial_board = create_playerboard(solution_board.xsize, solution_board.ysize)
//   println("initial_board:")
//   initial_board.print_board

//   val loc = convert_input_coordinates(game_input.reveal)
//   val current_board = reveal_more(solution_board, initial_board, loc) 
//   println("current_board:")
//   current_board.print_board
  
// def flagged_equals_mines(solution_board: SolutionBoard, player_board: PlayerBoard): Boolean = 
  // val mines_pos = solution_board.tile_map.filter((pos, tile) => tile == SolutionTile.Mine).keys
  // mines_pos.foldLeft(true)((acc, pos) => acc && player_board.tile_map(pos) == PlayerTile.Flagged