def convert_input_coordinate (input_coordinate: InputCoordinate): Coordinate = 
  Coordinate(input_coordinate.row - 1, input_coordinate.column - 1)

// * Converts InputCoordinate into Coordinate which corresponds to
// InputCoordinate.row - 1 = Coordinate.x 
// InputCoordinate.column - 1 = Coordinate.y
// ** You give
// input_coordinates : List of InputCoordinates parsed from input file
// ** You get
// List of Coordinates
def convert_input_coordinates (input_coordinates: List[InputCoordinate]): List[Coordinate] = 
  input_coordinates.map(ic => convert_input_coordinate(ic))


// * Prints boards in the matrix form (Array of Arrays)
// ** You give
// board : Array of Arrays of type T, where T can be Int for input board or String for Tile
// ** You get
// print out of the board  
def print[T](board: Array[Array[T]]): Unit = 
    println(board.map(_.mkString("")).mkString("\n"))
    println("\n")

