package numberguessing

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.net.ServerSocket
import scala.util.{Using, Random}
import common.network.*

object NumberGuessingServer extends App {
  Using(new ServerSocket(4444)) { server =>
    println("Server started, waiting for clients to connect...")
    val client = server.accept()
    println("Client connected")
    
    Using(new BufferedInputStream(client.getInputStream)) { in =>
      Using(new BufferedOutputStream(client.getOutputStream)) { out =>
        var game = new_game(MAX_ATTEMPTS)
        
        // send number of attempts
        send_data[Int](out, game.max_attempts)

        while !is_gameover(game) do {
          val player_guess = read_data[PlayerGuess](in)
          println(s"Player guessed: ${player_guess.number}")

          game.state = play(game, player_guess)
          var server_response = game_to_response(game)
          
          send_data[ServerResponse](out, server_response)
          println(s"sent: $server_response")
        } // while
      } // Using Out
    } // Using In
  } // Using Socket
  match {
    // If the block runs without any exceptions, Success is returned
    case scala.util.Success(_) => println("Server stopped successfully.")
    case scala.util.Failure(e) => println(s"An error occurred: ${e.getMessage}")
  }
}
