import scala.annotation.tailrec
// (0 until 3).flatMap(x => (0 until 3).map (y => (x, y)))

// for {
//     i <- (0 until 3)
//     j <- (0 until 3)
// } yield (i , j)

// val x = List(1,2,3).filter(_> 1)

enum Stream[A]:
    case StreamEmpty()
    case StreamCons(first: A, rest: () => Stream[A])

def make_evens_eq_or_greater(n : Int) : Stream[Int] = {
    Stream.StreamCons(n, () => make_evens_eq_or_greater(n + 2))
}


val all_even_numbers = make_evens_eq_or_greater(0)


def stream_first[A](s : Stream[A]) : Option[A] = {
    s match {
        case Stream.StreamEmpty() => None
        case Stream.StreamCons(first, rest) => Some(first)
    }
}

def stream_rest[A](s : Stream[A]) : Option[Stream[A]] = {
    s match {
        case Stream.StreamEmpty() => None
        case Stream.StreamCons(first, rest) => Some(rest())
    }
}

def stream_take[A](s : Stream[A], n : Int) : Option[List[A]] = {
    n match {
        case 0 => Some(List())
        case n => s match {
            case Stream.StreamEmpty() => None
            case Stream.StreamCons(first, rest) => for { 
                res <- stream_take(rest(), n - 1)
            } yield first :: res
            // The `for` is syntactic sugar for doing this:
            // stream_take(rest(), n - 1).flatMap(res => Some(first :: rest))
        }
    }
}

def stream_map[A, B](s : Stream[A], f: (A => B)) : Stream[B] = {
    s match {
        case Stream.StreamEmpty() => Stream.StreamEmpty()
        case Stream.StreamCons(first, rest) => Stream.StreamCons(f(first), () => stream_map(rest(), f))
    }
}

///////////////////////////////// Helpers ///////////////////////////////
def make_numbers_eq_or_greater(start: Int): Stream[Int] = {
  Stream.StreamCons(start, () => make_numbers_eq_or_greater(start + 1))
}

val all_positive_numbers = make_numbers_eq_or_greater(1)
val ten_positive_numbers = my_stream_take(all_positive_numbers, 10)
stream_to_string(ten_positive_numbers)

stream_to_string(stream_filter(ten_positive_numbers, x => x < 4))

// Takes a stream and returns a new stream containing the first n elements 
// Examples:
//   my_stream_take(all_even_numbers, 5) // (0, 2, 4, 6, 8)
def my_stream_take[A](s : Stream[A], n : Int) : Stream[A] = {
  n match {
      case 0 => Stream.StreamEmpty()
      case n => s match {
          case Stream.StreamEmpty() => Stream.StreamEmpty()
          case Stream.StreamCons(first, rest) => Stream.StreamCons(first, () => my_stream_take(rest(), n - 1))
    }
  }
}

val five_even_numbers = my_stream_take(all_even_numbers, 5)
stream_to_string(five_even_numbers)

def stream_reverse[A](stream: Stream[A]): Stream[A] = {
  def go(s: Stream[A], acc: Stream[A]): Stream[A] = {
    s match {
      case Stream.StreamEmpty() => acc
      case Stream.StreamCons(first, rest) => go(rest(), Stream.StreamCons(first, () => acc))
    }
  }
  go(stream, Stream.StreamEmpty())
}

// go((0, 2, 4, 6, 8, ()), ())
// go((2, 4, 6, 8, ()), (0, ()))
// go((4, 6, 8, ()), (2, (0, ())))  
// go((6, 8, ()), (4, (2, (0, ())))
// go((8, ()), (6, (4, (2, (0, ())))))
// go((), (8, 6, 4, 2, 0, ()))

stream_to_string(stream_reverse(five_even_numbers))
stream_to_string(stream_reverse(ten_positive_numbers))

// Takes a stream and turns it into a string so that it is easy to check what's in the stream for testing
// Examples:
//  stream_to_string[Int](five_even_numbers) // "0, 2, 4, 6, 8,"
//  stream_to_string[Int](Stream.StreamEmpty())  // ""
def stream_to_string[A](stream: Stream[A]): String = {
  stream match {
      case Stream.StreamEmpty() => ""
      case Stream.StreamCons(first, rest) => first.toString ++ ", " ++ stream_to_string[A](rest())
  }
}

stream_to_string[Int](Stream.StreamEmpty())
stream_to_string[Int](five_even_numbers)


// Takes a stream and returns the tail of the stream without `head_size` many elements in the front
// Examples:
//   stream_tail(ten_even_numbers, 3) // ()
def stream_tail[A](s: Stream[A], head_size: Int): Stream[A] = {
  head_size match {
    case 0 => s match {
      case Stream.StreamEmpty() => Stream.StreamEmpty()
      case _ => s
    }
    case n => s match {
      case Stream.StreamEmpty() => Stream.StreamEmpty()
      case Stream.StreamCons(first, rest) => stream_tail(rest(), head_size - 1)
    }
  }
}

val ten_even_numbers = my_stream_take(all_even_numbers, 10)
stream_to_string(ten_even_numbers)
val seven_tail_of_ten_even_numers = stream_tail(ten_even_numbers, 3)
stream_to_string(seven_tail_of_ten_even_numers)

// Takes a stream and turns it into a list
// Note: may not terminate for infinite streams
// Example:
//  stream_to_list(five_even_numbers) // List(0, 2, 4, 6, 8)
def stream_to_list[A](s: Stream[A]): List[A] = {
  s match {
    case Stream.StreamEmpty() => List()
    case Stream.StreamCons(first, rest) => first :: stream_to_list(rest())
  }
}

// Takes a stream and returns its size
// Note: may not terminate for infinite streams
// Example:
//  stream_size(five_even_numbers) // 5
def stream_size[A](s: Stream[A]): Int = {
  s match {
    case Stream.StreamEmpty() => 0
    case Stream.StreamCons(first, rest) => 1 + stream_size(rest())
  }
}

stream_size(ten_positive_numbers)
///////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////
// TODO: Dec 11

// Returns true if there exists an element, e, in Stream, s, such that p(e) is true
// Otherwise, false.
// Note: may not terminate for infinite-sized streams.
//
// Example:
// stream_exists(all_even_numbers, x => x % 2 == 0) // true
// stream_exists(all_even_numbers, x => x == 3) // does not terminate for `my_stream_exists`
// stream_exists(stream_take(all_even_numbers, 5), x => x == 3) // false
// def stream_exists_stack_overflow[A](s: Stream[A], p: A => Boolean): Boolean = {
//     s match {
//         case Stream.StreamEmpty() => false
//         case Stream.StreamCons(first, rest) => p(first) || stream_exists(rest(), p)
//     }
// }

// stream_exists tail recursive version
// it avoids stack overlow 
def stream_exists[A](stream: Stream[A], p: A => Boolean): Boolean = {
  @tailrec
  def go(s: Stream[A]): Boolean = {
    s match {
      case Stream.StreamEmpty() => false
      case Stream.StreamCons(first, rest) => 
        if p(first) then true
        else go(rest())
    }
  }
  go(stream)
}

// Tests
stream_exists[Int](all_even_numbers, x => x % 2 == 0) 
// stream_exists(five_even_numbers, x => x == 3)
// stream_exists(all_even_numbers, x => x == 3)

// Returns a subset of Stream s that satisfies a predicate p.
// Note: may not terminate for infinite-sized streams.
def stream_filter[A](stream: Stream[A], p: A => Boolean): Stream[A] = {
  @tailrec
  def go(s: Stream[A], acc: Stream[A]): Stream[A] = {
    s match {
      case Stream.StreamEmpty() => acc
      case Stream.StreamCons(first, rest) => p(first) match {
        case true => go(rest(), Stream.StreamCons(first, () => acc))
        case false => go(rest(), acc)
      }
    }
  }
  val reversed = go(stream, Stream.StreamEmpty())
  stream_reverse(reversed)
}

// stream_filter(all_even_numbers, x => x <= 10)  // will not terminate
stream_to_string(stream_filter(ten_positive_numbers, x => x < 7))

// Takes a stream and group the elements of the stream in `size`
// Example: 
//   val ten_positive_numbers = my_stream_take(all_positive_numbers, 10)
//   stream_group(ten_positive_numbers, 3) = ((1,2,3), (4,5,6), (7,8,9), (10))
def stream_group[A](s: Stream[A], size: Int): Stream[List[A]] = {
  s match {
    case Stream.StreamEmpty() => Stream.StreamEmpty()
    case Stream.StreamCons(first, rest) => rest() match {
      case Stream.StreamEmpty() => Stream.StreamCons(List(first), () => Stream.StreamEmpty())
      case Stream.StreamCons(first, rest) => stream_take(s, size) match {
        case None => Stream.StreamCons(stream_to_list(s), () => Stream.StreamEmpty()) // left over case
        case Some(l) => Stream.StreamCons(l, () => stream_group(stream_tail(s, size), size))
      }        
    }
  }
}

stream_to_string(stream_group(ten_positive_numbers, 3))


// returns a stream containing indicies of all the elements, e, in s where p(e) = true
def stream_index_where[A](s: Stream[A], p: A => Boolean): Stream[Int] = ???

// find the two smallest primes bigger than 100?
// Checkout LazyList
