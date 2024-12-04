package numberguessing

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.net.ServerSocket
import scala.util.{Using, Random}
import common.network.*

object NumberGuessingServer extends App {
  // val server = new ServerSocket(4444)
  Using(new ServerSocket(4444)) { server =>
    println("Server started, waiting for clients to connect...")
    val client = server.accept()
    client.setSoTimeout(10)
    println("Client connected")

  // val in = new BufferedInputStream(client.getInputStream)
  // val out = new BufferedOutputStream(client.getOutputStream)
  
    Using(new BufferedInputStream(client.getInputStream)) { in =>
      Using(new BufferedOutputStream(client.getOutputStream)) { out =>
        var game = new_game(MAX_ATTEMPTS)
            
        // send number of attempts
        send_data[Int](out, game.max_attempts)

        while !is_gameover(game) do {
          val player_guess = read_data_timeout[PlayerGuess](in, 5000)
          player_guess match {
            case Some(guess) => { 
              println(s"Player guessed: ${guess.number}")
              game.state = play(game, guess)
              var server_response = game_to_response(game)
            
              send_data[ServerResponse](out, server_response)
              println(s"sent: $server_response")
            }
            case None => {
              send_data[ServerResponse](out, ServerResponse.TimeOut)
              game.state = GameState.Lose
            }
          }
        } // while
        // println("out of while loop - server")
      } // Using Out
    } // Using In
  } // Using Socket
  match {
    // If the block runs without any exceptions, Success is returned
    case scala.util.Success(_) => println("Server stopped successfully.")
    case scala.util.Failure(e) => println(s"An error occurred: ${e.getMessage}")
  }
}
