package numberguessing

import scala.io.StdIn.readLine
import common.network.{send_data, read_data}
import java.io.{BufferedInputStream, BufferedOutputStream}

// TODO: server will shut the game down if human player takes too long to guess
// TODO: test this behavior 
// TODO: Third version multiplayer numberguessing game

def print_start(max_attempts: Int): Unit = 
    println("Welcome to Number Guessing Game.")
    println(s"Guess a number between $MIN_BETWEEN and $MAX_BETWEEN in $max_attempts tries.")

def print_response(response: ServerResponse): Unit = {
  response match {
    case ServerResponse.Wrong(Hint.Smaller) => println(s"Guess a smaller number.")
    case ServerResponse.Wrong(Hint.Bigger) => println(s"Guess a bigger number.")
    case ServerResponse.Correct => println("You guessed correctly!")
    case ServerResponse.Result(answer) => println(s"You lost. The answer was $answer")
    case _ => throw new WrongResponseException
  }
}

def print_result(g: Game): Unit = 
  g.state match {
    case GameState.Win => println("You win!")
    case GameState.Lose => println(s"You lose! The correct number was ${g.number}")
    case GameState.Continue(attempts_remaining, hint) => {
      val common_msg = s"You have $attempts_remaining attempt(s) left.\n"
      val hint_msg = hint match {
        case Hint.None => "No hint this time."
        case Hint.Bigger => s"Guess a bigger number."
        case Hint.Smaller => s"Guess a smaller number."
      }
      println(common_msg + hint_msg)
    }
  }

def parse_and_validate_guess(user_input: String): Either[String, PlayerGuess] = {
  try { 
    val x = user_input.toInt
    if  x > MIN_BETWEEN && x < MAX_BETWEEN then
      Right(PlayerGuess(user_input.toInt))
    else 
      Left(s"Enter a number between $MIN_BETWEEN and $MAX_BETWEEN")
  } catch {
    case e: NumberFormatException => Left("Enter a valid number.")
    case e: Exception => Left(s"Exception caught: ${e.getMessage}")
  }
}