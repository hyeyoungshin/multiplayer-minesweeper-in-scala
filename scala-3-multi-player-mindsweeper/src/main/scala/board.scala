// * Represents tile positions on the boards
//
// ** You give
// x : starting from top left corner (0, 0) grow to the right 
// y : starting from top left corner (0, 0) grow downards
// ** You get
// A tile position on the board
case class Coordinate (val x: Int, val y: Int)


case class Board[Tile] (val xsize: Int, val ysize:Int, val get_tile: Map[Coordinate, Tile])


type SolutionBoard = Board[SolutionTile]
type PlayerBoard = Board[PlayerTile]
type MineBoard = Board[Boolean]


// add purpose statement
enum SolutionTile:
  case Empty
  case Mine
  case Hint (val hint: Int)


enum PlayerTile:
  case Hidden 
  case Revealed (val tile: SolutionTile)


// * Reveals the corresponding SolutionTile at Coordinate on PlayerBoard 
//
// ** You give
// solutionboard : board with all tiles revealed (solution tiles)
// playerboard : player's version of board
// pos : tile position on playerboard to reveal (player click)
// * You get
// PlayerBoard with tile at pos revealed
def reveal(solutionboard: SolutionBoard, playerboard: PlayerBoard, pos: Coordinate): PlayerBoard = 
  val new_tile_map = playerboard.get_tile + (
    pos -> PlayerTile.Revealed (tile = solutionboard.get_tile(pos))
  )
  
  Board(
      xsize = playerboard.xsize,
      ysize = playerboard.ysize,
      get_tile = new_tile_map
  )


def neighbors_inbounds(xsize: Int, ysize: Int, all_neighbors: List[Coordinate]): List[Coordinate] = 
    all_neighbors.filter(pos => pos.x >= 0 && pos.x < xsize && pos.y >= 0 && pos.y < ysize)


def all_neighbors(pos: Coordinate): List[Coordinate] = 
    List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).map(
      (i, j) => Coordinate(pos.x + i, pos.y + j))



def count_neighboring_mines(mineboard: MineBoard, pos: Coordinate): Int = 
  val neighbors = neighbors_inbounds(mineboard.xsize, mineboard.ysize, all_neighbors(pos))

  neighbors.foldLeft(0)( (acc, pos) => if mineboard.get_tile(pos) then acc + 1 else acc )



// * Make SolutionTile at pos based on the number of neighboring 
//
// ** You give
// mineboard : mine locations 
// pos : Coordinate on Board
// * You get
// SolutionTile at Coordinate
def get_solutiontile_at(mineboard: MineBoard, pos: Coordinate): SolutionTile = 
  val num_mines = count_neighboring_mines(mineboard, pos)
  if mineboard.get_tile(pos) then
    SolutionTile.Mine

  else if num_mines == 0 then
    SolutionTile.Empty
  else 
    SolutionTile.Hint(num_mines)


// * Helper to generate Keys for Board.get_tile
// * For a board of size 3 * 3, it generates (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 1)
//
// ** You give
// xlen : Board's horizontal size
// ylen : Board's vertical size
// * You get
// A sequence of Keys for Board.get_tile
def coordinate_range (xlen: Int, ylen: Int) : IndexedSeq[(Int, Int)] = 
  (0 until xlen).flatMap( x => (0 until ylen).map( y => (x, y) ) )


// * Creates SolutionBoard from MineBoard at the start of game
// * Derives Hint from the mine locations in MineBoard
//
// ** You give
// mineboard: mine locations
// ** You get
// SolutionBoard
def create_solutionboard(mineboard: MineBoard): SolutionBoard =
  val range = coordinate_range(mineboard.xsize, mineboard.ysize) 
    
  Board(
    xsize = mineboard.xsize,
    ysize = mineboard.ysize,
    get_tile = range.map( (x, y) => ( Coordinate(x, y), get_solutiontile_at(mineboard, Coordinate(x, y)) ) ).toMap
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

  val range = coordinate_range(xlen, ylen)
  
  Board(
    xsize = xlen,
    ysize = ylen,
    get_tile = range.map( (x, y) => ( Coordinate(x, y), mine_locations(x)(y) == 1 )).toMap
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

  val range = coordinate_range(xlen, ylen)
  
  Board(
    xsize = xlen,
    ysize = ylen,
    get_tile = range.map( (x, y) => ( Coordinate(x, y), PlayerTile.Hidden )).toMap
  )

@main def hello(): Unit =
  //val input_board = [[0, 0, 1], [0, 0, 0],[0, 0, 1]]
  // val solutionboard = SolutionBoard(
  //   xsize = 3, 
  //   ysize = 3, 
  //   get_tile = Map(Coordinate(1, 1) -> SolutionTile.Empty, Coordinate(1, 2) -> SolutionTile.Empty, Coordinate(1, 3) -> SolutionTile.Mine,
  //   Coordinate(2, 1) -> SolutionTile.Empty, Coordinate(2, 2) -> SolutionTile.Empty, Coordinate(2, 3) -> SolutionTile.Hint(2),
  //   Coordinate(3, 1) -> SolutionTile.Empty, Coordinate(3, 2) -> SolutionTile.Empty, Coordinate(3, 3) -> SolutionTile.Mine
  //   )
  // )  
  
  // val playerboard = PlayerBoard(
  //   xsize = 3,
  //   ysize = 3,
  //   get_tile = Map(Coordinate(1, 1) -> PlayerTile.Hidden, Coordinate(1, 2) -> PlayerTile.Hidden, Coordinate(1, 3) -> PlayerTile.Hidden,
  //   Coordinate(2, 1) -> PlayerTile.Hidden, Coordinate(2, 2) -> PlayerTile.Hidden, Coordinate(2, 3) -> PlayerTile.Hidden,
  //   Coordinate(3, 1) -> PlayerTile.Hidden, Coordinate(3, 2) -> PlayerTile.Hidden, Coordinate(3, 3) -> PlayerTile.Hidden
  //   )
  // )

  // val newboard = reveal(solutionboard, playerboard, Coordinate(2, 3))
  // val newtile = newboard.get_tile(Coordinate(2, 3))

  // newtile match {
  //   case PlayerTile.Revealed(tile) => tile match {
  //     case SolutionTile.Hint(hint) if hint == 2 => println("yay!")
  //     case _ => println("boo!")
  //   }
  // }
  val game_input = parse_game_input("src/test/board_tests/1-in.json")
  
  // val mine_locations = Array(Array(0, 0, 1), Array(0, 0, 0), Array(0, 0, 1))
  println(game_input.board.length)
  // println(count_mine(mine_locations, 1, 2)) // 0 based indexing

  

def msg = "I was compiled by Scala 3. :)"

