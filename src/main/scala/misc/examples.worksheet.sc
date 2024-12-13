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

all_even_numbers match {
    case Stream.StreamCons(first, rest) => rest()
}

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

// TODO: Dec 11
// Checks if there exists an element, e, in Stream s that p(e) is true
// Scala uses short-circuit evaluation for the logical OR operator || 
// i.e., If p(first) is true, it won't evaluate the recursive call.
// Example:
// stream_exists(all_even_numbers, x => x % 2 == 0) // true
// stream_exists(all_even_numbers, x => x == 3) // stackoverflow
// def stream_exists[A](s: Stream[A], p: A => Boolean): Boolean = {
//     s match {
//         case Stream.StreamEmpty() => false
//         case Stream.StreamCons(first, rest) => p(first) || stream_exists(rest(), p)
//     }
// }

def stream_exists[A](s: Stream[A], p: A => Boolean): Boolean = {
  @tailrec
  def go(stream: Stream[A]): Boolean = {
    stream match {
      case Stream.StreamEmpty() => false
      case Stream.StreamCons(first, rest) => 
        if (p(first)) true
        else go(rest())
    }
  }
  
  go(s)
}

stream_exists(all_even_numbers, x => x % 2 == 0) 
stream_exists(all_even_numbers, x => x == 3)

// proof of short-circuit evaluation of OR
def expensiveComputation(): Boolean = {
  println("Expensive computation executed")
  true
}

// This will only evaluate the first part
val result1 = true || expensiveComputation()

// This will evaluate both parts
val result2 = false || expensiveComputation()


def stream_filter[A](s: Stream[A], p: A => Boolean): Stream[A] = ???

// s = (1,2,3,4,5,6,7,8,9,10)
// stream_group(s, 3) = ((1,2,3), (4,5,6), (7,8,9), (10))
def stream_group[A](s: Stream[A], size: Int): Stream[List[A]] = ???

// returns a stream containing indicies of all the elements, e, in s where p(e) = true
def stream_index_where[A](s: Stream[A], p: A => Boolean): Stream[Int] = ???

// find the two smallest primes bigger than 100?
// Checkout LazyList





val five_even = stream_take(all_even_numbers, 5) 
val all_odd_numbers = stream_map(all_even_numbers, x => x + 1)

stream_take(all_odd_numbers, 5)

