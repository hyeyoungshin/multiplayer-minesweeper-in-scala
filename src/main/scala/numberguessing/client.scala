package numberguessing

import java.net.Socket
import scala.util.Random
import java.io.{BufferedInputStream, BufferedOutputStream}
import common.network.*
import scala.io.StdIn.readLine

object NumberGuessingClient extends App {
  val socket = new Socket("localhost", 4444)
  println("Connected to Number Guessing game.")
  
  val in = new BufferedInputStream(socket.getInputStream)
  val out = new BufferedOutputStream(socket.getOutputStream)
  
  // user interface can be handled in client, but not good idea to mix with protocol 
  // with json data/scala type, messages that the client print vs.in data to compute with will be clearer
  var num_attempts = read_data[Int](in)
  
  while(num_attempts > 0) {
    println(s"number of attempts left: $num_attempts")

    val player_input = readLine()
    val player_guess = PlayerGuess(player_input.toInt)
    
    send_data(out, player_guess)
    println(s"Sent my guess: ${player_guess.number}")
    
    val server_response = read_data[ServerResponse](in)
    print_response(server_response)
    
    num_attempts = server_response match {
      case ServerResponse.Correct => 0
      case _ => num_attempts - 1
    }
  }

  socket.close()
}
