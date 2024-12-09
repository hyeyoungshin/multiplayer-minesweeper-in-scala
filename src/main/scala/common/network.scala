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


// The `send_data` function sends data of type `T` over the network
// The data is serialized into JSON format before sending
// The data is sent in two parts:
// 1. the size of the data is sent in 4 bytes
// 2. the actual data is sent
// The size of the data is used to read the actual data from the InputStream
// See `read_data_internal` for more details
def send_data[T : Writer](out: OutputStream, data: T): Unit = {
  val json_data = write(data)
  val data_size = get_json_data_size(json_data)
  val byte_data = json_data.getBytes("UTF-8")
  
  out.write(data_size)
  out.write(byte_data)
  out.flush()
}


// The `get_json_data_size` function computes the data size of `json_data` 
// in the byte array format and returns the size in the byte array format
// The size is computed in 4 bytes
def get_json_data_size(json_data: String): Array[Byte] = {
  val byte_size = json_data.getBytes("UTF-8").length
  ByteBuffer.allocate(4).putInt(byte_size).array()
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


def read_data_timeout[T : Reader](in: InputStream, timeout : Int): Option[T] = {
  run_with_timeout(check_timeout => read_data_internal(in, check_timeout), timeout)
}


// The `read_by_bytes` function reads exactly `num_bytes` bytes from the InputStream `in`
// and sometimes does so within a period of time specified by the `check_timeout` function
// The function reads the bytes in a loop until it reads `num_bytes` bytes
//  - The function reads `num_bytes - bytesRead` bytes in each iteration
//  - The function returns an array of bytes of size `num_bytes`
//    containing the bytes read from the InputStream
// There are tow mechanisms to ensure data reading takes within the allotted time:
// - `setSoTimeout` is called on client's socket to set the timeout for waiting to receive data
//    so that trickling partial data doesn't cause delay too much
// - `check_timeout` is called to make sure reading the complete data doesn't take longer than allotted time
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

// The `run_with_timeout` function  takes a function `task` and a `timeout` value in miliseconds
// If the task completes within the timeout, the result is returned as `Some(result)`
// Otherwise, None
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
