def fib(n: Int): Int = n match {
    case 1 => 0
    case 2 => 1
    case _ => fib(n-1) + fib(n-2)
  }

//fib(10)

List(1,2,3,4).length

def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def loop(n: Int): Boolean = {
    if (n+1 >= as.length)  true
    else if (ordered(as(n), as(n+1))) false
    else loop(n+1)
  }

  loop(0)
}

isSorted[Int](Array(1,2,4,3,5), (a: Int, b: Int) => a>b)


def curry[A,B,C] (f: (A,B) => C):A => (B => C) = a => b => f(a, b)

def curry2[A,B,C] (f: (A,B) => C):A => (B => C) = f.curried

def uncurry[A,B,C] (f:A=>B=>C):(A,B) => C = (a,b) => f(a)(b)

def compose[A,B,C] (f:B=>C, g:A=>B):A=>C = a => f(g(a))

def compose2[A,B,C] (f:B=>C, g:A=>B):A=>C = g andThen f