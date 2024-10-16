import java.net.ServerSocket
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.PrintStream

@main def network(): Unit = {
    val server_socket = new ServerSocket(4444)
    val socket = server_socket.accept()  // "blocking call" 
                            // the program waits until something makes a connection 
                            // once that happens, it gives us back a socket_1 which has the ability to connect to client
                            // a representation of a connection
                            // can be closed once the connection is closed



    val input_stream = new BufferedInputStream(socket.getInputStream()) // slow so use buffer?
    val output_stream = new PrintStream(new BufferedOutputStream(socket.getOutputStream()))

    output_stream.println("Hi there.") // kept in the buffer. force flush is needed. 
    output_stream.flush()

    // "busy wait" or
    while(input_stream.available() < 1) { Thread.sleep(100) } // check 10 times per second

    val buf = new Array[Byte](input_stream.available)
    
    input_stream.read(buf)
    
    val input = new String(buf)
    println(input)
    
    output_stream.println(input)
    output_stream.flush()
}