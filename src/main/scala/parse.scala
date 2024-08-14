import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import upickle.default.*

// The data representation mirroring json input
//
// * Input
// board: mine locations where 1 means there is a mine
// reveal: a list of tile coordinates to reveal
// * Output
// The data representation for json input data for parsing
case class GameInput (board: Array[Array[Int]], reveal: List[InputCoordinate]) derives ReadWriter

// The data representation for input coordinates
// 1-based indexing
// i.e.,
// [(1,1)][(1,2)][(1,3)]
// [(2,1)][(2,2)][(2,3)]
// [(3,1)][(3,2)][(3,3)]
//
// * Input
// row: row number
// column: column number
// * Output
// The representation of user input coordinate
case class InputCoordinate(row: Int, column: Int) derives ReadWriter


def convert_input_coordinate_to_coordinate(input: InputCoordinate): Coordinate = {
  // 1,1 -> 0,0
  // 1,2 -> 1,0
  // 1,3 -> 2,0
  // 1,4 -> 3,0
  Coordinate(input.column - 1, input.row - 1)
}


def convert_coordinate_to_input_coordinate(tile_pos: Coordinate): InputCoordinate = {
  // 0,0 -> 1,1
  // 1,0 -> 1,2
  // 2,0 -> 1,3
  // 3,0 -> 1,4
  InputCoordinate(tile_pos.y + 1, tile_pos.x + 1)
}
  

// Takes a json file and returns GameInput which contains
// 1. board : mine locations in a matrix represented by array of arrays where 1 means there is a mine
// 2. reveal : the tiles to reveal
def parse_game_input(filename: String): GameInput = {
  val path = Paths.get(filename)
  val json_str = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
  
  read[GameInput](json_str)
}
