package ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("error")
    case Cons(_, xs) => xs
  }

  def setHead[A](h: A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_,xs) => Cons(h, xs)
  }

  def drop[A](n: Int, list: List[A]): List[A] =
    if (n==0) list
    else list match {
      case Nil => Nil
      case Cons(_, xs) => drop(n-1, xs)
    }

  def apply[A](as: A*):List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))
  }
}