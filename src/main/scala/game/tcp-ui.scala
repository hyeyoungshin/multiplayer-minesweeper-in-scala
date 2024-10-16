import scala.io.StdIn.readLine
import java.awt.Color
import upickle.implicits.Readers
import java.io.BufferedOutputStream

// def get_valid_input[T](message: String, parse_and_validate: String => Either[String, T]): T = {
//   def loop(): T = 
//     print_with_effect(message, PrinterEffects.Bold)
//     val player_input = readLine()
//     parse_and_validate(player_input) match {
//       case Right(value) => value
//       case Left(msg) => print_with_effect(msg, PrinterEffects.Red); loop()
//     }
  
//   loop()
// }

// // 
// def get_valid_inputs(state: GameState): PlayerAction = {
//   def loop(): PlayerAction = 
//     val coordinate = get_valid_coordinate(state)
//     val action = get_valid_action()
//     valid_player_action(state, PlayerAction(action, coordinate)) match {
//       case Right(action) => action
//       case Left(msg) => print_with_effect(msg, PrinterEffects.Bold); loop()
//     }

//   loop()
// }

def get_valid_response[T](msg: String, out: BufferedOutputStream): T = {
    ???
}