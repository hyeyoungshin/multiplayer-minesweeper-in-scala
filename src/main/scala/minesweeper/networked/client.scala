package minesweeper.network

import java.net.Socket
import java.nio.ByteBuffer
import scala.util.Random
import java.io.{BufferedInputStream, BufferedOutputStream}
import common.network.*

object MinesweeperClient extends App {
  val socket = new Socket("localhost", 4444)
  println("Connected to Multi-player Minesweeper game.")
  
  val in = new BufferedInputStream(socket.getInputStream)
  val out = new BufferedOutputStream(socket.getOutputStream)

  val welcome_message = read_data[String](in)
  println(welcome_message)

  send_data(out, 2)

  val choose_difficulty = read_data[String](in)
  println(choose_difficulty)
  
  send_data(out, "Easy")

  socket.close()
}