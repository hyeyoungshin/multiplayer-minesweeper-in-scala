package numberguessing

import java.net.Socket
import scala.util.Random
import java.io.{BufferedInputStream, BufferedOutputStream}
import common.network.*
import scala.io.StdIn.readLine
import minesweeper.get_valid_input


object NumberGuessingClient extends App {
  val socket = new Socket("localhost", 4444)
  println("Connected to Number Guessing game.")
  
  val in = new BufferedInputStream(socket.getInputStream)
  val out = new BufferedOutputStream(socket.getOutputStream)
  
  // user interface can be handled in client, but not good idea to mix with protocol 
  // with json data/scala type, messages that the client print vs.in data to compute with will be clearer
  var remaining_attempts = read_data[Int](in)
  print_start(remaining_attempts)
  
  while(remaining_attempts > 0) {
    println(s"number of attempts left: $remaining_attempts")

    // TODO (Oct 30) Handle server timeout situation
    // 1. easier: catch "server closed" exception and print something out
    // 2. maybe harder: server sends a ServerResponse type of data (e.g. TimeOut) before closing
    //                  client reads it 
    // Need to understand Socket behaviors better
    // Need to create a tiny example to learn the behaviors
    // Maybe need concurreny
    // (Nov 20)
    // make it two player game 
    
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
  }
  // println("out of while loop")
  in.close()
  out.close()
  socket.close()
  println("Connection closed.")
}
