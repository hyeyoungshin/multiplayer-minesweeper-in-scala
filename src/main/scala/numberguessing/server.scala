package numberguessing

import java.io._
import java.nio.ByteBuffer
import java.net.ServerSocket
import scala.util.Using
import scala.util.Random
import java.util.Arrays
import upickle.default.*
import common.network.*

object NumberGuessingServer extends App {
  Using(new ServerSocket(4444)) { server =>
    println("Server started, waiting for clients to connect...")
    val client = server.accept()
    println("Client connected")

    // Use BufferedReader for input and PrintWriter for output
    Using(new BufferedInputStream(client.getInputStream)) { in =>
      Using(new BufferedOutputStream(client.getOutputStream)) { out =>
        val game_number = Random.between(1, 100)
        println(s"Game number: $game_number")
        
        var num_tries = 3

        while(num_tries > 0) {
          val player_guess = read_data[PlayerGuess](in)
          println(s"Player guessed: ${player_guess.guess}")

          if player_guess.guess == game_number then {
            num_tries = 0
          } else if player_guess.guess < game_number then {
            send_data(out, ServerResponse.Wrong(Hint.BiggerThan))
            num_tries -= 1
          } else {
            send_data(out, ServerResponse.Wrong(Hint.SmallerThan))
            num_tries -= 1
          }
        }
        send_data(out, ServerResponse.Correct(game_number))
      }
    }
  } match {
    // If the block runs without any exceptions, Success is returned
    case scala.util.Success(_) => println("Server stopped successfully.")
    case scala.util.Failure(e) => println(s"An error occurred: ${e.getMessage}")
  }
}
