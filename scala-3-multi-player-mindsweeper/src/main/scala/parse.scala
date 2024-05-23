import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import upickle.default.*

case class GameInput (board: Array[Array[Int]], reveal: List[InputCoordinate]) derives ReadWriter
// 1-based indexing
case class InputCoordinate(row: Int, column: Int) derives ReadWriter

// Takes a json file and returns GameInput which contains
// 1. (mine)board : location of mines in a matrix represented by Array of Arrays
// 2. reveal : location of a tile in the matrix
def parse_game_input(filename: String): GameInput = 
  val path = Paths.get(filename)
  val json_str = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
  
  read[GameInput](json_str)

  


  
  



