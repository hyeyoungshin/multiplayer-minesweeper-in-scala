package guessing_game

import scala.util.Random
import scala.io.StdIn.readLine

val MAX_ATTEMPTS = 5
val MIN_BETWEEN = 0 
val MAX_BETWEEN = 99

enum GameState:
  case Win
  case Continue(val attempts_remaining: Int, val answer: Int)//, val flag: Boolean)
  case Lose

def new_game(): GameState = 
  GameState.Continue(MAX_ATTEMPTS, Random.between(MIN_BETWEEN, MAX_BETWEEN))

def play(g: GameState, guess: Int): GameState = 
    g match {
        case GameState.Continue(a, n) => {
            if n == guess then 
                GameState.Win 
            else if a > 1 then 
                GameState.Continue(a - 1, n)
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
    case GameState.Continue(a, _) => println(s"You have $a attempts left. Guess again.")
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

// model  (game data, separation of game logic from view and controller)
// view (take instance of model and present it eg. webpage) 
// controller (takes user input and trigger model and update the view)
