package minesweeper

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import upickle.default.*
import javax.swing.text.html.HTML.Tag
import java.awt.TrayIcon.MessageType
import minesweeper.game.*

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


//////////////////////////////////////
///////// Messaging via TCP  /////////
//////////////////////////////////////

enum GameMessage derives ReadWriter:
  case GameBoard(width: Int, height: Int, tile_matrix: Array[Array[Tile]])
  case GameAction(tile_pos: InputCoordinate, action: String)
  case PlayerTurn(id: Int)
  case GameError(msg: String)
  case GameResult(winner: Int) // winner: Player

// enum GameError derives ReadWriter:
//   case FullGame
//   case GameExists
//   case NoExistingGame


case class Tile(hint: Option[Int], flagged_by: Option[Int], is_mine: Boolean) derives ReadWriter

// mineboard
// [ ][ ][ ]
// [*][ ][ ]
// [ ][*][ ]

// solution board
// [1][1][0]
// [x][2][1]
// [2][x][1]

// player board
// [_][ _ ][0]
// [x][F|2][_]
// [2][ F ][_]

// [x] => {"hint": None, "flagged_by": None, "is_mine": true}
// [_] => {"hint": None, "flagged_by": None, "is_mine": false}
// [F] => {"hint": None, "flagged_by": Some(0), "is_mine": false}
// [F|0] => {"hint": Some(0), "flagged_by": Some(0), "is_mine": false}




///////////////////////////////////////
////////////// Parsing ////////////////
///////////////////////////////////////


// Takes a json file and returns GameMessage which contains
// 1. $type : the type of message 
// 2. Message : content of the message, can be one of Board, Action, Turn, Error, and Result
def parse_game_message(filename: String): GameMessage = {
  val path = Paths.get(filename)
  val json_str = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
  read[GameMessage](json_str)
}


def parse_game_board(gm: GameMessage): Either[String, PlayerBoard]= {
  gm match {
    case GameMessage.GameBoard(width, height, tile_matrix) => Right(Board[PlayerTile](height, width, parse_tile_matrix(tile_matrix)))
    case _ => Left("Not GameBoard.")
  }
}


def parse_game_action(gm: GameMessage): Either[String, PlayerAction] = {
  gm match {
    case GameMessage.GameAction(tile_pos, action) => parse_str_action(action) match {
      case Right(parsed_action) => Right(PlayerAction(parsed_action, convert_input_coordinate_to_coordinate(tile_pos)))
      case Left(error) => Left(error)
    }
    case _ => Left("Not GameAction.")
  }
}


def parse_str_action(action: String): Either[String, Action] = {
  action match {
    case "R" => Right(Action.Reveal)
    case "F" => Right(Action.Flag)
    case "U" => Right(Action.Unflag)
    case _ => Left("Enter R for Reveal, F for Flag, or U for Unflag.")
  }
}

def parse_turn(gm: GameMessage): Either[String, Player] = {
  gm match {
    case GameMessage.PlayerTurn(id) => Right(Player(id))
    case _ => Left("Not Turn.")
  }
}

def parse_result(gm: GameMessage): Either[String, Player] = {
  gm match {
    case GameMessage.GameResult(id) => Right(Player(id))
    case _ => Left("Not Result.")
  }
}

// def parse_error(gm: GameMessage): Either[String, GameError] = {
//   Error
// }


def write_something(gm: GameMessage): String = {
  write(gm)
}

// def parse_message_board(message: Message): Option[PlayerBoard] = {
//   message match {
//     case Message.Board(width, height, tile_matrix) => Some(Board(height, width, parse_tile_matrix(tile_matrix)))
//     case _ => None
//   }
// }


def parse_tile_matrix(tile_matrix: Array[Array[Tile]]): Map[Coordinate, PlayerTile] = {
  val playertile_matrix = tile_matrix.map(arr => arr.map(tile => parse_tile(tile)))
  val keys = generate_coordinate_keys(tile_matrix(0).length, tile_matrix.length)
  
  keys.foldRight(Map())((key, acc) => acc + (key -> playertile_matrix(key.y)(key.x)))
} 

def parse_tile(tile: Tile): PlayerTile = {
  tile match {
    case Tile(_, _, true) => PlayerTile.Revealed(SolutionTile.Mine)
    case Tile(None, None, false) => PlayerTile.Hidden
    case Tile(None, Some(id), false) => PlayerTile.Flagged(Player(id))
    case Tile(Some(num), None, false) => PlayerTile.Revealed(SolutionTile.Hint(num))
    case Tile(Some(num), Some(id), false) => PlayerTile.RevealedNFlagged(SolutionTile.Hint(num), Player(id))
  }
}
  

@main def parse(): Unit =
  // val gm = GameMessage.GameAction(InputCoordinate(1, 1), "R")
  val board = GameMessage.GameBoard(2, 2, Array(Array(Tile(None, None, false), Tile(Some(1), None, false)),
                                                Array(Tile(Some(1), Some(0), false), Tile(None, None, true))))

  // println(write_something(board))
  
  // println(write(GameMessage.PlayerTurn(2)))
  case class Guess(number: Int)  derives ReadWriter 
  println(write(Guess(12)))
  


///////////////////////////////////////
//////////// Conversions //////////////
///////////////////////////////////////

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
