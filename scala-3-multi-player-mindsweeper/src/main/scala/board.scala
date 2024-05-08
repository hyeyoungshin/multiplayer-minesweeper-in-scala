// x starting from top left corner (0, 0) grow to the right 
// y starting from top left corner (0, 0) grow downards
case class Coordinate (val x: Int, val y: Int):
  def equals(location: (Int, Int)) = location match {
    case (x, y) => this.x == x && this.y == y
    case _ => false
  }
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

@main def hello(): Unit =
  //val input_board = [[0, 0, 1], [0, 0, 0],[0, 0, 1]]
  val solutionboard = SolutionBoard(
    xsize = 3, 
    ysize = 3, 
    tile_map = Map(Coordinate(1, 1) -> SolutionTile.Empty, Coordinate(1, 2) -> SolutionTile.Empty, Coordinate(1, 3) -> SolutionTile.Mine,
    Coordinate(2, 1) -> SolutionTile.Empty, Coordinate(2, 2) -> SolutionTile.Empty, Coordinate(2, 3) -> SolutionTile.Hint(2),
    Coordinate(3, 1) -> SolutionTile.Empty, Coordinate(3, 2) -> SolutionTile.Empty, Coordinate(3, 3) -> SolutionTile.Mine
    )
  )  
  
  val playerboard = PlayerBoard(
    xsize = 3,
    ysize = 3,
    tile_map = Map(Coordinate(1, 1) -> PlayerTile.Hidden, Coordinate(1, 2) -> PlayerTile.Hidden, Coordinate(1, 3) -> PlayerTile.Hidden,
    Coordinate(2, 1) -> PlayerTile.Hidden, Coordinate(2, 2) -> PlayerTile.Hidden, Coordinate(2, 3) -> PlayerTile.Hidden,
    Coordinate(3, 1) -> PlayerTile.Hidden, Coordinate(3, 2) -> PlayerTile.Hidden, Coordinate(3, 3) -> PlayerTile.Hidden
    )
  )

  val newboard = reveal(solutionboard, playerboard, Coordinate(2, 3))
  val newtile = newboard.tile_map(Coordinate(2, 3))

  newtile match {
    case PlayerTile.Revealed(tile) => tile match {
      case SolutionTile.Hint(hint) if hint == 2 => println("yay!")
      case _ => println("boo!")
    }
  }

def msg = "I was compiled by Scala 3. :)"

