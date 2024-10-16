// package numberguessing
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import upickle.default._  
import org.scalatest.funsuite.AnyFunSuite


class Network extends AnyFunSuite {
    test("data sent is same as data received") {
        val data = PlayerGuess(10)
        
        // val json_data = write(data)
        // val byte_data = json_data.getBytes("UTF-8")
        // val size_of_json_data_in_bytes = byte_data.length
        
        val out = new ByteArrayOutputStream()
        send_data(out, data)
        // out.write(byte_data)
        val data_off_out = out.toByteArray

        val in = new ByteArrayInputStream(data_off_out)

        val data_off_in = read_data[PlayerGuess](in)
        // val buf = new Array[Byte](data_off_out.length)
        // in.read(buf)
        // val data_off_in = new String(buf, "UTF-8")
        // val read_data = read[PlayerGuess](data_off_in)
        
        assert(data_off_in.guess == 10)
    }

    def read_from_file[T](filename: String)(using reader: Reader[T]): T = {
        val path = Paths.get(filename)
        val json_str = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
        read[T](json_str)
    }

    test("from-file") {
        val data_location = "src/test/json/number-guessing/guess.json"
        val data = read_from_file[PlayerGuess](data_location)
        val out = new ByteArrayOutputStream()
        send_data(out, data)
        val data_from_file = out.toByteArray()

        val in = new ByteArrayInputStream(data_from_file)
        val result = read_data[PlayerGuess](in)

        assert(data.guess == result.guess)
    }
}