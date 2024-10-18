package numberguessing

import scala.util.Random
import scala.io.StdIn.readLine
import minesweeper.get_valid_input


def new_game(max_attempts: Int): Game = 
  Game(Random.between(MIN_BETWEEN, MAX_BETWEEN), max_attempts, GameState.Continue(max_attempts, Hint.None))


def parse_guess(user_input: String): Either[String, PlayerGuess] = {
  try { 
    Right(PlayerGuess(user_input.toInt))
  } catch {
    case e: NumberFormatException => Left("Enter a valid number.")
    case e: Exception => Left(s"Exception caught: ${e.getMessage}")
  }
}

// def get_valid_guess(parsed: Either[String, PlayerGuess]): PlayerGuess = {
//   def loop(): PlayerGuess = 
//     parsed match {
//       case Right(guess) => guess
//       case Left(e) => print(e); loop()
//     }

//   loop()
// }

def play(game: Game, guess: PlayerGuess): GameState = 
    game.state match {
        case GameState.Continue(attempts_remaining, _) => {
            if guess.number == game.number then
                GameState.Win
            else if attempts_remaining == 1 && guess.number != game.number then
                GameState.Lose
            else if attempts_remaining > 1 && guess.number < game.number then 
                GameState.Continue(attempts_remaining - 1, Hint.Bigger)
            else
                GameState.Continue(attempts_remaining - 1, Hint.Smaller)
        }
        case _ => throw new IllegalStateException()
    }

def is_gameover(g: Game): Boolean = 
    g.state match {
        case GameState.Lose => true
        case GameState.Win => true
        case _ => false
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
    case _ => throw new IllegalStateException()
  }

def game_to_response(g: Game): ServerResponse = {
    g.state match {
      case GameState.Continue(_, hint) => ServerResponse.Wrong(hint)
      case GameState.Win => ServerResponse.Correct
      case GameState.Lose => ServerResponse.Result(g.number)
    }
}

def print_response(response: ServerResponse): Unit = {
  response match {
    case ServerResponse.Wrong(Hint.Smaller) => println(s"Guess a smaller number.")
    case ServerResponse.Wrong(Hint.Bigger) => println(s"Guess a bigger number.")
    case ServerResponse.Correct => println("You guessed correctly!")
    case ServerResponse.Result(answer) => println(s"You lost. The answer was $answer")
    case _ => throw new WrongResponseException
    }
}

def print_start(): Unit = 
    println("\n")
    println("Welcome to Number Guessing Game.\n")
    println(s"Guess a number between $MIN_BETWEEN and $MAX_BETWEEN.\n")

@main def guessing_game(): Unit = 
  print_start()
  var game = new_game(5)
  while !is_gameover(game) do
    val user_guess = get_valid_input("Enter a number:", parse_guess)
    game.state = play(game, user_guess)
    print_result(game)

// **Note
// model:  game data, separation of game logic from view and controller
// view: take instance of model and present it eg. webpage) 
// controller: takes user input and triggers model and updates the view
