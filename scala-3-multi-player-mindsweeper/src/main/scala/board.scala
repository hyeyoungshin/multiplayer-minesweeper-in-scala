// x starting from top left corner (0, 0) grow to the right 
// y starting from top left corner (0, 0) grow downards
case class Coordinate (val x: Int, val y: Int):
  // def equals(location: (Int, Int)) = location match {
  //   case (x, y) => this.x == x && this.y == y
  //   case _ => false
  // }
end Coordinate


// for testing, should be able to create a small board
case class SolutionBoard (val xsize: Int, val ysize:Int, val tile_map: Map[Coordinate, SolutionTile]):
end SolutionBoard


case class PlayerBoard (val xsize: Int, val ysize: Int, val tile_map: Map[Coordinate, PlayerTile]):
end PlayerBoard


// class Player(val name: String, var flags: Int = 10, var points: Int)
// class Tile

// enum Revealed extends Tile:
//   case Empty
//   case Mine
//   case Hint (val hint: Int)
// end Revealed


enum SolutionTile:
  case Empty
  case Mine
  case Hint (val hint: Int)


enum PlayerTile:
  case Hidden 
  case Revealed (val tile: SolutionTile)


def reveal(solutionboard: SolutionBoard, playerboard: PlayerBoard, coordinate: Coordinate): PlayerBoard = 
  val new_tile_map = playerboard.tile_map + (
    coordinate -> PlayerTile.Revealed(solutionboard.tile_map(coordinate))
  )
  
  PlayerBoard(
      xsize = playerboard.xsize,
      ysize = playerboard.ysize,
      tile_map = new_tile_map
  )


case class TilePos (val x: Int, val y: Int)


def valid_neighbors(xsize: Int, ysize: Int, all_neighbors: List[TilePos]): List[TilePos] = 
    all_neighbors.filter(pos => pos.x > -1 && pos.x < xsize && pos.y > -1 && pos.y < ysize)


def all_neighbors(pos: TilePos): List[TilePos] = 
    List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).map(
      (i, j) => TilePos(pos.x + i, pos.y + j))


def count_mine(mine_locations: Array[Array[Int]], x: Int, y: Int): Int = 
  val pos = TilePos(x, y)
  val neighbors = valid_neighbors(mine_locations.length, mine_locations.head.length, all_neighbors(TilePos(x, y)))

  neighbors.foldLeft(0)((acc, pos) => acc + mine_locations(pos._1)(pos._2))


def create_tile(mine_locations: Array[Array[Int]], x: Int, y: Int): SolutionTile = 
  val num_mines = count_mine(mine_locations, x, y)
  if mine_locations(x)(y) == 1 then
    SolutionTile.Mine
  else if num_mines == 0 then
    SolutionTile.Empty
  else 
    SolutionTile.Hint(num_mines)


def create_board(mine_locations: Array[Array[Int]]): SolutionBoard =
  val xlen = mine_locations.length
  val ylen = mine_locations.head.length

  val coordinates = (0 until xlen).flatMap( r => (0 until ylen).map( c => (r, c) ) )
    
  SolutionBoard(
    xsize = xlen,
    ysize = ylen,
    tile_map = coordinates.map( (r, c) => (Coordinate(r, c), create_tile(mine_locations, r, c)) ).toMap
  )


@main def hello(): Unit =
  //val input_board = [[0, 0, 1], [0, 0, 0],[0, 0, 1]]
  // val solutionboard = SolutionBoard(
  //   xsize = 3, 
  //   ysize = 3, 
  //   tile_map = Map(Coordinate(1, 1) -> SolutionTile.Empty, Coordinate(1, 2) -> SolutionTile.Empty, Coordinate(1, 3) -> SolutionTile.Mine,
  //   Coordinate(2, 1) -> SolutionTile.Empty, Coordinate(2, 2) -> SolutionTile.Empty, Coordinate(2, 3) -> SolutionTile.Hint(2),
  //   Coordinate(3, 1) -> SolutionTile.Empty, Coordinate(3, 2) -> SolutionTile.Empty, Coordinate(3, 3) -> SolutionTile.Mine
  //   )
  // )  
  
  // val playerboard = PlayerBoard(Assos MILLE GT Bib Shorts
  //   xsize = 3,
  //   ysize = 3,
  //   tile_map = Map(Coordinate(1, 1) -> PlayerTile.Hidden, Coordinate(1, 2) -> PlayerTile.Hidden, Coordinate(1, 3) -> PlayerTile.Hidden,
  //   Coordinate(2, 1) -> PlayerTile.Hidden, Coordinate(2, 2) -> PlayerTile.Hidden, Coordinate(2, 3) -> PlayerTile.Hidden,
  //   Coordinate(3, 1) -> PlayerTile.Hidden, Coordinate(3, 2) -> PlayerTile.Hidden, Coordinate(3, 3) -> PlayerTile.Hidden
  //   )
  // )

  // val newboard = reveal(solutionboard, playerboard, Coordinate(2, 3))
  // val newtile = newboard.tile_map(Coordinate(2, 3))

  // newtile match {
  //   case PlayerTile.Revealed(tile) => tile match {
  //     case SolutionTile.Hint(hint) if hint == 2 => println("yay!")
  //     case _ => println("boo!")
  //   }
  // }
  val game_input = parse_game("src/test/board_tests/1-in.json")
  
  // val mine_locations = Array(Array(0, 0, 1), Array(0, 0, 0), Array(0, 0, 1))
  println(game_input.board.length)
  // println(count_mine(mine_locations, 1, 2)) // 0 based indexing

  

def msg = "I was compiled by Scala 3. :)"

