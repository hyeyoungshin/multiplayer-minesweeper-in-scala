package numberguessing

import java.net.Socket
import java.nio.ByteBuffer
import scala.util.Random
import java.io.{BufferedInputStream, BufferedOutputStream}
import common.network.*


object NumberGuessingClient extends App {
  val socket = new Socket("localhost", 4444)
  println("Connected to Number Guessing game.")
  
  val in = new BufferedInputStream(socket.getInputStream)
  val out = new BufferedOutputStream(socket.getOutputStream)
  
  // user interface can be handled in client, but not good idea to mix with protocol 
  // with json data/scala type, messages that the client print vs.in data to compute with will be clearer
  var num_tries = 3
  var max = 100
  var min = 0
  
  while(num_tries > 0) {
    // Compute a guess
    val my_guess = PlayerGuess(Random.between(min, max))
    
    send_data(out, my_guess)
    println(s"Sent my guess: ${my_guess.guess}")

    val server_response = read_data[ServerResponse](in)

    server_response match {
      case ServerResponse.Wrong(Hint.SmallerThan) => num_tries -= 1; max = my_guess.guess; println(s"Guess a smaller number.")
      case ServerResponse.Wrong(Hint.BiggerThan) => num_tries -= 1; min = my_guess.guess; println(s"Guess a bigger number.")
      case ServerResponse.Correct(_) => num_tries = 0;
    }
  }

  val answer = read_data[ServerResponse](in)
  
  answer match {
    case ServerResponse.Correct(answer) => println(s"The correct answer was $answer.")
    case _ => throw new WrongResponseException

  }

  socket.close()
}
