package numberguessing

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.net.{ServerSocket, Socket}
// import scala.util.{Using, Random}
import common.network.*
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

class NumberGuessingServer {
  private val serverSocket = ServerSocket(4444)
  // to manage player list and server capacity
  private var players: List[Socket] = List()
  private val MAX_PLAYERS = 2

  def start(): Unit = {
    println("Starting the server...")

    while(true) {
      val clientSocket = serverSocket.accept()
      println("player connected!")

      players = clientSocket :: players

      if (players.size <= MAX_PLAYERS)
        // handle each player in a separate thread
        Future {
          handleClient(clientSocket)
        }
      else 
        val out = new BufferedOutputStream(clientSocket.getOutputStream)
        send_data[ServerMessage](out, ServerMessage("Server is full. Please try again later."))
        out.close()
        clientSocket.close()
        serverSocket.close()
    }
  }
}

def handleClient(clientSocket: Socket): Unit = {
  val in = BufferedInputStream(clientSocket.getInputStream)
  val out = new BufferedOutputStream(clientSocket.getOutputStream)

  // game logic for individual client
  var game = new_game(MAX_ATTEMPTS)
  send_data[Int](out, game.max_attempts)

  while !is_gameover(game) do {
    val player_guess = read_data_timeout[PlayerGuess](in, 10000)
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
  in.close()
  out.close
  clientSocket.close()
}


object ServerApp extends App {
  val server = NumberGuessingServer()
  server.start()
}
