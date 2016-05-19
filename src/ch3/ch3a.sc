import ch3._

def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
  as match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs, z)(f))
  }
}

//foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))


foldRight(List(1,2,3), 0)((a,b)=> 1)