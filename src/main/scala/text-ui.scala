import scala.io.StdIn.readLine

def print_start(): Unit = 
  println("\n")
  println("Welcome to the minesweeper game. \n")
  println("Enter your name: ")
  val user_name = readLine()
  println("\n")
  println(s"Hello, $user_name! Starting a game with \n")
  println(s"Board size: $BOARD_SIZE \n")
  println(s"Number of mines: $NUM_MINES \n")
