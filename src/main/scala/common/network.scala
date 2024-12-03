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

def read_by_bytes_test(in: InputStream, num_bytes: Int): Array[Byte] = {
  val buffer = new Array[Byte](num_bytes)
  var bytesRead = 0 // how many bytes we've read so far
  while (bytesRead < num_bytes) {
    println(s"sleeping for 3 second(s) during the task...")
    Thread.sleep(3000)
    // `num_bytes - bytesRead`: how many more bytes left to read
    val result = in.read(buffer, bytesRead, num_bytes - bytesRead)
    if (result == -1) throw new RuntimeException("End of stream reached unexpectedly")
    bytesRead += result
  }
  
  buffer
}

// Uses two runnables to run a task with a time limit
// 1. worker thread runs the task passed to the thunk
// 2. until interrupter thread interrupts the worker after the time limit
// 3. join method blocks the main thread until the worker thread completes the task
// If it finishes within the time limit, it returns Some(result of the task)
// Otherwise, None
def run_with_timeout[A](task: => A, timeout: Int): Option[A] = {
  val runnable_worker = MyWorker[A](task)
  val worker = Thread(runnable_worker)

  val runnable_interrupter = MyInterrupter(timeout, worker) 
  val interrupter = Thread(runnable_interrupter)

  worker.start()      // 1. starts the task passed to thunk 
  interrupter.start() // 2. waits for timeout miliseconds before interrupting the workder
  worker.join()       // 3. waits for the worker to finish
  
  runnable_worker.result
}


// WorkerReader is a class that takes a thunk `f` and stores the result of the task in `result`
class MyWorker[A](f: => A) extends Runnable {
  var result: Option[A] = None
  def run(): Unit =
    try {
      result = Some(f)
    } catch {
      case e: InterruptedException => None
    }
}

// WorkerReader is a class that takes a thunk `f` and stores the result of the task in `result`
class MyInterrupter(time: Int, worker: Thread) extends Runnable {
  def run(): Unit =
    println("interrupter sleeping...")
    Thread.sleep(time)
    println("interrupter interrupting...")
    worker.interrupt() // if worker doesn't exists (worker completed its task), it has no effect
}
