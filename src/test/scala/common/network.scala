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

    test("from-file") {
        val data_location = "src/test/json/number-guessing/guess.json"
        val data = read_from_file[TestGuess](data_location)
        assert(data.guess == write_and_read(data).guess)
    }
}