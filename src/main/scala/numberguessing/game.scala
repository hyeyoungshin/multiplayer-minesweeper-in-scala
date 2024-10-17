package numberguessing

import scala.util.Random
import scala.io.StdIn.readLine


def new_game(): GameState = 
  GameState.Continue(MAX_ATTEMPTS, Random.between(MIN_BETWEEN, MAX_BETWEEN))

def play(g: GameState, guess: Int): GameState = 
    g match {
        case GameState.Continue(attempts_remaining, answer) => {
            if answer == guess then 
                GameState.Win 
            else if attempts_remaining > 1 then 
                GameState.Continue(attempts_remaining - 1, answer)
            else
                GameState.Lose
        }
        case _ => throw new IllegalStateException()
    }

def is_gameover(g: GameState): Boolean = 
    g match {
        case GameState.Lose => true
        case GameState.Win => true
        case _ => false
    }

def print_state(g: GameState): Unit = 
  g match {
    case GameState.Win => println("You win!")
    case GameState.Lose => println("You lose!")
    case GameState.Continue(attempts_remaining, _) => println(s"You have $attempts_remaining attempts left. Guess again.")
  }

def print_start(): Unit = 
    println("\n")
    println("Let's play a number guessing game.\n")
    println(s"I will choose a number between $MIN_BETWEEN and $MAX_BETWEEN.\n")
    println(s"Guess what it is. You have $MAX_ATTEMPTS attempts allowed.")

@main def guessing_game(): Unit = 
    print_start()
    var state = new_game()
    while !is_gameover(state) do
        val user_guess = readLine().toInt
        state = play(state, user_guess)
        print_state(state)

// model:  game data, separation of game logic from view and controller
// view: take instance of model and present it eg. webpage) 
// controller: takes user input and triggers model and updates the view
