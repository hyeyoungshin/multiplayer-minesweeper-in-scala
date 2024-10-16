import java.io._
import java.nio.ByteBuffer
import java.net.ServerSocket
import scala.util.Using
import scala.util.Random
import java.util.Arrays
import upickle.default.*

case class GameStart(num_players: Int)


object MinesweeperServer extends App {
  Using(new ServerSocket(4444)) { server =>
    println("Server started, waiting for clients to connect...")
    val client = server.accept()
    println("Client connected")

    Using(new BufferedInputStream(client.getInputStream)) { in =>
      Using(new BufferedOutputStream(client.getOutputStream)) { out =>
        val welcome_msg = "How many players?"
        send_data(out, welcome_msg)

        val num_players = read_data[Int](in)
        println(s"- number of players: $num_players")

        // val players = (0 until num_players).map(id => Player(id))
        val player = Player(0)
        

        val ask_difficulty = "Choose difficulty: Easy, Medium, Hard"
        send_data(out, ask_difficulty)
        
        val difficulty = read_data[String](in)
        println(s"- difficulty: ${difficulty}")
        
        val mineboard = difficulty match {
            case "Easy" => create_mineboard_for_game(Easy)
            case "Medium" => create_mineboard_for_game(Medium)
            case "Hard" => create_mineboard_for_game(Hard)
            case _ => create_mineboard_for_game(Easy) // default difficulty is easy 
                                                      // TODO: get_valid_response
        }
        
        // val solution_board = create_solutionboard(mineboard)
        val player_board = create_playerboard(mineboard.xsize, mineboard.ysize)
        val player_state = PlayerState(player, player_board)
        
        print_board(player_board)

        // send_data(out, player_board)
      }
    }
  }
}
