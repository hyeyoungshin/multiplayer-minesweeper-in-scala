(0 until 3).flatMap(x => (0 until 3).map (y => (x, y)))

for {
    i <- (0 until 3)
    j <- (0 until 3)
} yield (i , j)

val mine_locations = Array(Array(1,2,3), Array(4,5,6), Array(7,8,9))

mine_locations(0)(0)
mine_locations(0)(1)
mine_locations(0)(2)

