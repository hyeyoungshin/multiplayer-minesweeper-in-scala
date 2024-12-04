package common.network

import org.scalatest.funsuite.AnyFunSuite
import upickle.default.*

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.io.InputStream

case class TestGuess(guess: Int) derives ReadWriter 

// For mocking input with data trickling in slowly that should trigger timeout
class SlowInputStream(wrapped: InputStream) extends InputStream {
    override def read(): Int = {
        Thread.sleep(10);
        wrapped.read()
    }
}

class Network extends AnyFunSuite {
    def write_and_read[T : ReadWriter](data : T): T = {
        val out = new ByteArrayOutputStream()
        send_data(out, data)
        val in = new ByteArrayInputStream(out.toByteArray)
        read_data(in)
    }

    def read_from_file[T : Reader](filename: String): T = {
        val path = Paths.get(filename)
        val json_str = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
        read(json_str)
    }

    test("data sent is same as data received") {
        assert(write_and_read(TestGuess(10)).guess == 10)
    }

    test("write two objects then read two objects") {
        val out = new ByteArrayOutputStream()
        send_data(out, TestGuess(10))
        send_data(out, TestGuess(9))
        val in = new ByteArrayInputStream(out.toByteArray)
        assert(read_data[TestGuess](in).guess == 10)
        assert(read_data[TestGuess](in).guess == 9)
    }

    test("from-file data sent is same as data received") {
        val data_location = "src/test/json/number-guessing/guess.json"
        val data = read_from_file[TestGuess](data_location)
        assert(data.guess == write_and_read(data).guess)
    }

    test("run_with_timeout_test should fail") {
        val out = new ByteArrayOutputStream()
        send_data(out, TestGuess(10))
        val in = new ByteArrayInputStream(out.toByteArray)
        println(out.toByteArray().length)

        // Reads one byte each 10 ms
        val slow = new SlowInputStream(in)
        
        // 16 bytes, one per 10ms should take 160 ms; less than that, so we should time out.
        val result = read_data_timeout[TestGuess](slow, 150)

        assert(result == None)
    }

    test("run_with_timeout should succeed") {
        val out = new ByteArrayOutputStream()
        send_data(out, TestGuess(10))
        val in = new ByteArrayInputStream(out.toByteArray)
        
        val result = read_data_timeout[TestGuess](in, 6000)

        result match {
            case Some(data) => assert(data == TestGuess(10))
            case None => fail("Timeout reached")
        }
    }


    // This doesn't interrupt and so runs forever!  
    // test("timeout doesn't work on most computations") {
    //     def do_loop() : Unit = {
    //         do_loop()
    //     }
    //     assert(run_with_timeout(do_loop(), 100) == None)
    // }

    
}