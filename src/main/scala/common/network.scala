package common.network

import upickle.default.*
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer

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


// Takes an input stream and reads two types of data from the input stream
// 1. data size
// 2. (actual) data
// Once it reads data size (which has the fixed size of 4 bytes), it translates it into Int,
// which is used to read the actual data 
def read_data[T : Reader](in: InputStream): T = {
  // [T: Reader] is a context parameter, meaning
  // the function requires an implicit Reader[T] to be available when the function is called
  // This is commonly used to summon type class instances (like Reader from uPickle).
  val data_size_in_bytes = read_by_bytes(in, 4)
  val data_size = ByteBuffer.wrap(data_size_in_bytes).getInt
  val data = read_by_bytes(in, data_size)

  read(data)
}


// Computes the data size of json_data in the byte array format 
// and returns the size in the byte array format
def get_json_data_size(json_data: String): Array[Byte] = {
  val byte_size = json_data.getBytes("UTF-8").length
  // println(s"sending ${byte_size} data...")
  ByteBuffer.allocate(4).putInt(byte_size).array()
}

// Helper that reads exactly `num_bytes` bytes from InputStream `in`
def read_by_bytes(in: InputStream, num_bytes: Int): Array[Byte] = {
  val buffer = new Array[Byte](num_bytes)
  var bytesRead = 0 // how many bytes we've read so far
  while (bytesRead < num_bytes) {
    // `num_bytes - bytesRead`: how many more bytes left to read
    val result = in.read(buffer, bytesRead, num_bytes - bytesRead)
    if (result == -1) throw new RuntimeException("End of stream reached unexpectedly")
    bytesRead += result
  }
  
  buffer
}

def read_by_bytes_sleep(in: InputStream, num_bytes: Int): Array[Byte] = {
  val buffer = new Array[Byte](num_bytes)
  var bytesRead = 0 // how many bytes we've read so far
  while (bytesRead < num_bytes) {
    println(s"sleeping for 5 second(s) during the task...")
    Thread.sleep(5000)
    // `num_bytes - bytesRead`: how many more bytes left to read
    val result = in.read(buffer, bytesRead, num_bytes - bytesRead)
    if (result == -1) throw new RuntimeException("End of stream reached unexpectedly")
    bytesRead += result
  }
  
  buffer
}

// Runs `task` for `timeout` milliseconds and returns the result of the task 
// If it finishes within the time limit, it returns the result of the task wrapped in Option type
// Otherwise, it returns None
def run_with_timeout[A](task: (InputStream, Int) => A, timeout: Int)(in: InputStream, num_bytes: Int): Option[A] =
  val runnable = WorkerReader[A](task, in, num_bytes)
  val worker = Thread(runnable)
  worker.start()
  println("running the task...")
  Thread.sleep(timeout)
  println(s"Timeout ${timeout/1000} seconds reached. Interrupting the worker...")
  worker.interrupt()
  runnable.result


// Designed to be used with `run_with_timeout`
// `run_with_timeout` is a higher-order function that takes a function `task` and Int `timeout`.
// It runs the task for timeout milliseconds and returns the result of the task if it finishes within the time limit.
// Otherwise, it returns None.
// WorkerReader's `result` stores the result of the task.
class WorkerReader[A](f: (InputStream, Int) => A, in: InputStream, num_bytes: Int) extends Runnable {
  var result: Option[A] = None
  def run(): Unit =
    try {
      result = Some(f(in, num_bytes))
    } catch {
      case e: InterruptedException => None
    }
}
