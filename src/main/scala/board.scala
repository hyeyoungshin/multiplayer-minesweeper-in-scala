/////////////////////////////
//// Data types for Board////
/////////////////////////////

case class Board[Tile] (val xsize: Int, val ysize:Int, val tile_map: Map[Coordinate, Tile]):
  
  def print_board: Unit = 
    val str_board = Array.fill(this.xsize)(Array.fill(this.ysize)(""))
    this.tile_map.map((tile_pos, tile) => str_board(tile_pos._1)(tile_pos._2) = tile.toString())
    print[String](str_board)

  def within_boundary(tile_pos: Coordinate): Boolean = 
    tile_pos.x > -1 && tile_pos.y > -1 && tile_pos.x < xsize && tile_pos.y < ysize

  def is_hidden(tile_pos: Coordinate): Boolean = 
    this.tile_map(tile_pos) == PlayerTile.Hidden

type SolutionBoard = Board[SolutionTile]
type PlayerBoard = Board[PlayerTile]
type MineBoard = Board[Boolean]

enum SolutionTile: 
  case Empty
  case Mine
  case Hint (hint: Int)
  override def toString() = this match {
    case Empty => "[E]"
    case Mine => "[x]"
    case Hint(n) => s"[$n]"
  }

enum PlayerTile:
  case Hidden
  case Revealed (tile: SolutionTile)
  case Flagged

  override def toString() = this match {
    case Hidden => "[ ]"
    case Revealed(t) => t.toString()
    case Flagged => "[F]"
  }

// * Represents tile positions on the boards
//
// ** You give
// x : starting from top left corner (0, 0) grow to the right 
// y : starting from top left corner (0, 0) grow downwards
// ** You get
// A tile position on the board
case class Coordinate (val x: Int, val y: Int)

////////////////////////
//// Creating Board/////
////////////////////////

// * Creates SolutionBoard from MineBoard at the start of game
// * Derives Hint from the mine locations in MineBoard
//
// ** You give
// mineboard: mine locations
// ** You get
// SolutionBoard
def create_solutionboard(mineboard: MineBoard): SolutionBoard =
  val range = generate_coordinate_keys(mineboard.xsize, mineboard.ysize) 
    
  Board(
    xsize = mineboard.xsize,
    ysize = mineboard.ysize,
    tile_map = range.map( (x, y) => ( Coordinate(x, y), generate_solutiontile_at(mineboard, Coordinate(x, y)) ) ).toMap
  )

// * Creates MineBoard which represents the mine locations from GameInput at the start of game
// * MineBoard is necessary for creating SolutionBoard 
//
// ** You give
// mine_locations : corresponds to GameInput.board from parse_game_input
// ** You get
// MineBoard where true means a mine; false no mine
def create_mineboard(mine_locations: Array[Array[Int]]): MineBoard = 
  val xlen = mine_locations.length
  val ylen = mine_locations.head.length

  val range = generate_coordinate_keys(xlen, ylen)
  
  Board(
    xsize = xlen,
    ysize = ylen,
    tile_map = range.map( (x, y) => ( Coordinate(x, y), mine_locations(x)(y) == 1 )).toMap
  )

// * Creates the initial PlayerBoard at the start of a game
//
// ** You give
// xlen : number of tiles horizontally
// ylen : number of tiles vertically
// ** You get
// PlayerBoard where all tiles are hidden
def create_playerboard(xsize: Int, ysize: Int): PlayerBoard = 
  val xlen = xsize
  val ylen = ysize

  val range = generate_coordinate_keys(xlen, ylen)
  
  Board(
    xsize = xlen,
    ysize = ylen,
    tile_map = range.map( (x, y) => (Coordinate(x, y), PlayerTile.Hidden)).toMap
  )


////////////////////////
//// Updating Board/////
////////////////////////

def update_board(playerboard: PlayerBoard, tile_pos: Coordinate, new_tile: PlayerTile): PlayerBoard = 
  Board(
    xsize = playerboard.xsize,
    ysize = playerboard.ysize,
    tile_map = playerboard.tile_map + (tile_pos -> new_tile)
  )  


// * Reveals the corresponding SolutionTile at Coordinate on PlayerBoard 
//
// ** You give
// solutionboard : board with all tiles revealed (solution tiles)
// playerboard : player's version of board
// tile_pos : tile position on playerboard to reveal (player click)
// * You get
// PlayerBoard with tile at tile_pos revealed
def reveal(solutionboard: SolutionBoard, playerboard: PlayerBoard, tile_pos: Coordinate): PlayerBoard = 
  // assuming playertile at tile_pos is Hidden
  // assuming solution_tile can be Mine which we deal later
  val solution_tile = solutionboard.tile_map(tile_pos)
  val updated_board = update_board(playerboard, tile_pos, PlayerTile.Revealed(solution_tile))
  
  solution_tile match {
    case SolutionTile.Empty => reveal_neighbors(solutionboard, updated_board, tile_pos)
    case SolutionTile.Mine => reveal_all_mines(solutionboard, updated_board)
    case _ => updated_board
  }

// * Reveals all Mines on PlayerBoard 
//
// ** You give
// solutionboard : board with all tiles revealed (solution tiles)
// playerboard : player's version of board
// tile_pos : tile position on playerboard to reveal (player click)
// * You get
// PlayerBoard with all mines revealed
def reveal_all_mines(solutionboard: SolutionBoard, playerboard: PlayerBoard): PlayerBoard =
  val filtered_map = solutionboard.tile_map.filter((tile_pos, tile) => tile == SolutionTile.Mine)
  
  filtered_map.keys.foldLeft(playerboard)((acc, tile_pos) => update_board(acc, tile_pos, PlayerTile.Revealed(SolutionTile.Mine)))


// Get neighboring tiles of tile_pos. Check what solutontiles corresponds to neighboring tiles
// If SolutionTile.Empty reveal_neighbors with updated playerboard at tile_pos
def reveal_neighbors(solutionboard: SolutionBoard, playerboard: PlayerBoard, tile_pos: Coordinate): PlayerBoard = 
  val neighbors = neighbors_inbounds(solutionboard, tile_pos)

  neighbors.foldLeft(playerboard)(
    (acc, tile_pos) =>  {
      if playerboard.tile_map(tile_pos) != PlayerTile.Hidden then
        acc
      else 
        reveal(solutionboard, acc, tile_pos)
    }
  )


def reveal_more(solutionboard: SolutionBoard, playerboard: PlayerBoard, loc: List[Coordinate]): PlayerBoard =
  loc.foldLeft(playerboard)((acc, tile_pos) => reveal(solutionboard, acc, tile_pos))


// TODO: Need to handle flagging Hidden or Revealed tiles better
// Currently aborts with "player action always results in new board."
def flag(playerboard: PlayerBoard, tile_pos: Coordinate): Option[PlayerBoard] = 
  playerboard.tile_map(tile_pos) match {
    case PlayerTile.Hidden => Some(update_board(playerboard, tile_pos, PlayerTile.Flagged))
    case PlayerTile.Revealed(_) => None
    case PlayerTile.Flagged => None
  }
  
// TODO: Need to handle unflagging Hidden or Revealed tiles better
// Currently aborts with "player action always results in new board."
def unflag(playerboard: PlayerBoard, tile_pos: Coordinate): Option[PlayerBoard] = 
  playerboard.tile_map(tile_pos) match {
    case PlayerTile.Hidden => None
    case PlayerTile.Revealed(_) => None
    case PlayerTile.Flagged => Some(update_board(playerboard, tile_pos, PlayerTile.Hidden))
  }


///////////////
//// Helper////
///////////////

def neighbors_inbounds[T](board: Board[T], tile_pos: Coordinate): List[Coordinate] = 
  val all_neighbors = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).map(
      (i, j) => Coordinate(tile_pos.x + i, tile_pos.y + j))
  
  // TODO: use within_boundary
  all_neighbors.filter(tile_pos => board.within_boundary(tile_pos))
    
    
def count_neighboring_mines(mineboard: MineBoard, tile_pos: Coordinate): Int = 
  val neighbors = neighbors_inbounds(mineboard, tile_pos)

  neighbors.foldLeft(0)( (acc, tile_pos) => if mineboard.tile_map(tile_pos) then acc + 1 else acc )


// * Make SolutionTile at tile_pos based on the number of neighboring mines
//
// ** You give
// mineboard : mine locations 
// tile_pos : Coordinate on Board
// * You get
// SolutionTile at Coordinate
def generate_solutiontile_at(mineboard: MineBoard, tile_pos: Coordinate): SolutionTile = 
  val num_mines = count_neighboring_mines(mineboard, tile_pos)
  if mineboard.tile_map(tile_pos) then
    SolutionTile.Mine

  else if num_mines == 0 then
    SolutionTile.Empty
  else 
    SolutionTile.Hint(num_mines)


// * Helper to generate Keys for populating Board.tile_map
// * For a board of size 3 * 3, it generates (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 1)
//
// ** You give
// xlen : Board's horizontal size
// ylen : Board's vertical size
// * You get
// A sequence of Keys for Board.tile_map
def generate_coordinate_keys (xlen: Int, ylen: Int) : IndexedSeq[(Int, Int)] = 
  (0 until xlen).flatMap( x => (0 until ylen).map( y => (x, y) ) )


/////////////////
//// Scratch ////
/////////////////

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