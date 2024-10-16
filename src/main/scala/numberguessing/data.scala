package numberguessing

import upickle.default.*

// {"guess": 12}
case class PlayerGuess(guess: Int)  derives ReadWriter 

// {"$type": "Wrong", "hint": {"$type": "SmallerThan"}}
// {"$type": "Correct", "answer": 3}
enum ServerResponse derives ReadWriter:
  case Wrong(hint: Hint)
  case Correct(answer: Int)

enum Hint derives ReadWriter:
  case SmallerThan
  case BiggerThan
  
class WrongResponseException extends Exception