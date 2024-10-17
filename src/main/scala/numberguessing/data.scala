package numberguessing

import upickle.default.*

val MAX_ATTEMPTS = 5
val MIN_BETWEEN = 0 
val MAX_BETWEEN = 99

enum GameState:
  case Win
  case Continue(val attempts_remaining: Int, val answer: Int)
  case Lose

// TODO: ef gamestate -> server_response

// {"guess": 12}
case class PlayerGuess(guess: Int)  derives ReadWriter 

// {"$type": "Wrong", "hint": {"$type": "SmallerThan"}}
// {"$type": "Correct", "answer": 3}
enum ServerResponse derives ReadWriter:
  case Wrong(hint: Hint)
  case Lose(answer: Int)
  case Correct
  
enum Hint derives ReadWriter:
  case SmallerThan
  case BiggerThan
  
class WrongResponseException extends Exception
