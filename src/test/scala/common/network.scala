package common.network

import org.scalatest.funsuite.AnyFunSuite
import upickle.default.*

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

case class TestGuess(guess: Int) derives ReadWriter 

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

    test("from-file timeout long enough") {
        val data_location = "src/test/json/number-guessing/guess.json"
        val data = read_from_file[TestGuess](data_location)
        assert(data.guess == write_and_read(data).guess)
    }

    test("run_with_timeout_test should fail") {
        val out = new ByteArrayOutputStream()
        send_data(out, TestGuess(10))
        val in = new ByteArrayInputStream(out.toByteArray)
        
        val data_size_in_bytes = read_by_bytes(in, 4)
        val data_size = java.nio.ByteBuffer.wrap(data_size_in_bytes).getInt
        val result = run_with_timeout[Array[Byte]](read_by_bytes_sleep(in, data_size), 2000)

        assert(result == None)
    }

    test("run_with_timeout should succeed") {
        val out = new ByteArrayOutputStream()
        send_data(out, TestGuess(10))
        val in = new ByteArrayInputStream(out.toByteArray)
        
        val data_size_in_bytes = read_by_bytes(in, 4)
        val data_size = java.nio.ByteBuffer.wrap(data_size_in_bytes).getInt
        val result = run_with_timeout(read_by_bytes_sleep(in, data_size), 6000)
        
        result match {
            case Some(data) => assert(read[TestGuess](data) == TestGuess(10))
            case None => fail("Timeout reached")
        }
    }

    test("what does read_data do now?") {
        val out = new ByteArrayOutputStream()
        send_data(out, "How many players?")
        val in = new ByteArrayInputStream(out.toByteArray)
        val data = read_data[String](in)
        assert(data == "How many players?")
    }
}