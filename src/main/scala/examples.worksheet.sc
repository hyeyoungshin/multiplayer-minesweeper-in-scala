(0 until 3).flatMap(x => (0 until 3).map (y => (x, y)))

for {
    i <- (0 until 3)
    j <- (0 until 3)
} yield (i , j)
