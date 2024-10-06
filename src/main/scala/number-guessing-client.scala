import java.net.Socket
import java.io._
import scala.util.Random


object NumberGuessingClient extends App {
  val socket = new Socket("localhost", 4444)
  println("Connected to Number Guessing game.")
  
  val in = new BufferedInputStream(socket.getInputStream)
  val out = new PrintStream(BufferedOutputStream(socket.getOutputStream))
  
  // user interface can be handled in client, but not good idea to mix with protocol 
  // with json data/scala type, messages that the client print vs.in data to compute with will be clearer
  var num_tries = 3
  
  while(num_tries > 0) {
    // Compute a guess
    val guess_number = Random.between(1, 100)

    // Send a guess to the server
    out.print(guess_number)
    out.flush()

    // Read the server's response
    while(in.available() < 1) { Thread.sleep(100) }
     
    // println(s"what is available number of bytes of data in the input stream?: ${in.available}")
    val buf = new Array[Byte](in.available)
    in.read(buf) 
    val server_response = new String(buf)
    println(server_response)

    if server_response.contains("wrong") then
        num_tries -= 1
    else 
        num_tries = 0
  }
  
  println("game over!")
  socket.close()
}
