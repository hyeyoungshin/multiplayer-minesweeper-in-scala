package numberguessing

import scala.util.Random
import scala.io.StdIn.readLine
import minesweeper.get_valid_input


def new_game(max_attempts: Int): Game = 
  Game(Random.between(MIN_BETWEEN, MAX_BETWEEN), max_attempts, GameState.Continue(max_attempts, Hint.None))


def play(game: Game, guess: PlayerGuess): GameState = 
    game.state match {
        case GameState.Continue(remaining_attempts, _) => {
            if guess.number == game.number then
                GameState.Win
            else if remaining_attempts == 1 && guess.number != game.number then
                GameState.Lose
            else if remaining_attempts > 1 && guess.number < game.number then 
                GameState.Continue(remaining_attempts - 1, Hint.Bigger)
            else
                GameState.Continue(remaining_attempts - 1, Hint.Smaller)
        }
        case GameState.Lose => throw new IllegalStateException()
        case GameState.Win => throw new IllegalStateException()
    }

def is_gameover(g: Game): Boolean = 
    g.state match {
        case GameState.Lose => true
        case GameState.Win => true
        case _ => false
    }

def game_to_response(g: Game): ServerResponse = {
    g.state match {
      case GameState.Continue(_, hint) => ServerResponse.Wrong(hint)
      case GameState.Win => ServerResponse.Correct
      case GameState.Lose => ServerResponse.Result(g.number)
    }
}

@main def guessing_game(): Unit = 
  print_start(MAX_ATTEMPTS)
  var game = new_game(5)
  while !is_gameover(game) do
    val user_guess = get_valid_input("Enter a number:", parse_and_validate_guess)
    game.state = play(game, user_guess)
    print_result(game)

// **Note
// model:  game data, separation of game logic from view and controller
// view: take instance of model and present it eg. webpage) 
// controller: takes user input and triggers model and updates the view
