package numberguessing

import java.net.Socket
import scala.util.Random
import java.io.{BufferedInputStream, BufferedOutputStream}
import common.network.*
import scala.io.StdIn.readLine
import minesweeper.get_valid_input

class NumberGuessigClient {
  private val socket = new Socket("localhost", 4444)
  private val in = new BufferedInputStream(socket.getInputStream)
  private val out = new BufferedOutputStream(socket.getOutputStream)
  
  // private val consoleIn = new BufferedReader(new InputStreamReader(System.in))

  def play(): Unit = {
    println("player connected!")
    
    var remaining_attempts = read_data[Int](in)
    print_start(remaining_attempts)

    while(remaining_attempts > 0) {
      println(s"number of attempts left: $remaining_attempts")

      // Client is responsible for sending valid data
      val player_guess = get_valid_input("Enter a number:", parse_and_validate_guess)
      send_data(out, player_guess)
      println(s"Sent my guess: ${player_guess.number}")
      
      val server_response = read_data[ServerResponse](in)
      server_response match {
        case ServerResponse.TimeOut => println("Timed out."); remaining_attempts = 0
        case _ => { 
          print_response(server_response)

          remaining_attempts = server_response match {
            case ServerResponse.Correct => 0
            case _ => remaining_attempts - 1
          }
        }
      }
    } // while
    socket.close()
  }
}

object ClientApp extends App {
  val client = NumberGuessigClient()
  client.play()
}
