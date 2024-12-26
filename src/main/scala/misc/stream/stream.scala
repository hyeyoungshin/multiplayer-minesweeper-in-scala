package stream

class Stream[A](head: A, tail: () => Stream[A]):

  def take(n: Int): List[A] = {
    n match {
      case 0 => List()
      case 1 => List(head)
      case n if n > 1 => head :: tail().take(n - 1)
      case _ => throw new IllegalArgumentException("Input must be >= 0")
    }
  }

  def map[B](f: A => B): Stream[B] = {
    new Stream(f(head), () => this.tail().map(f))
  }

  def filter(p: A => Boolean): Stream[A] = {
    if p(head) then
      new Stream(head, () => tail().filter(p))
    else
      // may run indefinitely
      tail().filter(p)
  }

  def drop(size: Int): Stream[A] = {
    size match {
      case 0 => this
      case n => tail().drop(n - 1)
    }
  }

  def group(size: Int): Stream[List[A]] = {
    new Stream(this.take(size), () => this.drop(size).group(size))    
  }
