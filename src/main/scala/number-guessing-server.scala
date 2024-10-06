import java.net.ServerSocket
// import java.io.BufferedReader
// import java.io.InputStreamReader
// import java.io.BufferedOutputStream
import java.io._
import scala.util.Using
import scala.util.Random

case class Guess(number: Int) // {"$type": "Guess", "num": 12}

enum Response:
  case Wrong
  case Correct

enum Hint: 
  case LessThan
  case BiggerThan


object NumberGuessingServer extends App {
  Using(new ServerSocket(4444)) { server =>
    println("Server started, waiting for clients to connect...")
    val client = server.accept()
    println("Client connected")

    // Use BufferedReader for input and PrintWriter for output
    Using(new BufferedInputStream(client.getInputStream)) { in =>
      Using(new PrintStream(new BufferedOutputStream(client.getOutputStream))) { out =>
        // Pick a number
        val game_number = Random.between(1, 100)
        // Print out game number
        println(s"Game number: $game_number")
        
        var num_tries = 3
        
        while(num_tries > 0) {
          // wait until there is data available in the input stream before trying to read from it
          while(in.available() < 1) { Thread.sleep(100) }

          // Read guess from the client and print it
          // if `in.available == 0`, then it will create an empty buffer, which ends up making `guess` an empty string
          // val buf = new Array is a fixed size buffer which can read up to 1024 bytes
          val buf = new Array[Byte](in.available)          
          in.read(buf) 
          val guess = new String(buf)
              
          println(s"Client guessed: $guess")
          out.println(s"You guessed: $guess") 
          
          if guess.toInt == game_number then 
            num_tries = 0

            out.println("You won!")
            out.flush()
          else
            num_tries -= 1

            out.println("You guessed wrong.")
            out.flush()

        }

        out.println(s"The game number was ${game_number}.")
        out.flush()
      }
    }
  } match {
    // If the block runs without any exceptions, Success is returned
    case scala.util.Success(_) => println("Server stopped successfully.")
    case scala.util.Failure(e) => println(s"An error occurred: ${e.getMessage}")
  }
}
