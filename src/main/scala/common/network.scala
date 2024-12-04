package common.network

import upickle.default.*
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer
import java.net.SocketTimeoutException
import java.util.concurrent.TimeoutException

// Notes from the meeting on Oct 9
    // implement def send_data(out, T) any T that has "Writer of T?
    // ByteArrayWriter might be useful for testing
    // create a library that deals with networking stuff using generic (interface) input/output stream
    // create testsuite for that 
    // property testing! 
    // instead of low-level testing, test properties that I care about

// Writes data to out and send 
def send_data[T : Writer](out: OutputStream, data: T): Unit = {
  val json_data = write(data)
  val data_size = get_json_data_size(json_data)
  val byte_data = json_data.getBytes("UTF-8")
  
  out.write(data_size)
  out.write(byte_data)
  out.flush()
}

def read_data_timeout[T : Reader](in: InputStream, timeout : Int): Option[T] = {
  run_with_timeout(check_timeout => read_data_internal(in, check_timeout), timeout)
}

def read_data[T : Reader](in: InputStream): T = {
  read_data_internal(in, () => {})
}

// Takes an input stream and reads two types of data from the input stream
// 1. data size
// 2. (actual) data
// Once it reads data size (which has the fixed size of 4 bytes), it translates it into Int,
// which is used to read the actual data 
def read_data_internal[T : Reader](in: InputStream, check_timeout: () => Unit): T = {
  // [T: Reader] is a context parameter, meaning
  // the function requires an implicit Reader[T] to be available when the function is called
  // This is commonly used to summon type class instances (like Reader from uPickle).
  val data_size_in_bytes = read_by_bytes(in, 4, check_timeout: () => Unit)
  val data_size = ByteBuffer.wrap(data_size_in_bytes).getInt
  val data = read_by_bytes(in, data_size, check_timeout: () => Unit)

  read(data)
}


// Computes the data size of json_data in the byte array format 
// and returns the size in the byte array format
def get_json_data_size(json_data: String): Array[Byte] = {
  val byte_size = json_data.getBytes("UTF-8").length
  ByteBuffer.allocate(4).putInt(byte_size).array()
}

// Helper that reads exactly `num_bytes` bytes from InputStream `in`
def read_by_bytes(in: InputStream, num_bytes: Int, check_timeout: () => Unit): Array[Byte] = {
  val buffer = new Array[Byte](num_bytes)
  var bytesRead = 0 // how many bytes we've read so far
  while (bytesRead < num_bytes) {
    // `num_bytes - bytesRead`: how many more bytes left to read
    // in.read will block until either a byte is read from the network, or the socket timeout is reached
    val result =
        try {
            in.read(buffer, bytesRead, num_bytes - bytesRead)
        } catch {
            case e: SocketTimeoutException => 0
        }
    if (result == -1) throw new RuntimeException("End of stream reached unexpectedly")
    bytesRead += result
    check_timeout()
  }
  
  buffer
}

def run_with_timeout[A](task: (() => Unit) => A, timeout: Int): Option[A] = {
    val begin = System.currentTimeMillis();
        
    def check_timeout() : Unit = {
        val current = System.currentTimeMillis()
        if (current - begin > timeout) {
            throw TimeoutException()
        }
    }

    try {
        Some(task(check_timeout))
    } catch {
        case e : TimeoutException => None
    }
}