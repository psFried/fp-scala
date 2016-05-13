package linkedlist

sealed abstract class SinglyLinkedList[+A] {

  def head(): A
  def tail(): SinglyLinkedList[A]

  @annotation.tailrec
  final def foldLeft[B](ident: B, f: (B, A) => B): B = {
    this match {
      case Nope => ident
      case Cons(h, t) => t.foldLeft(f(ident, h), f)
    }
  }

  def replaceHead[B >: A](a: B): SinglyLinkedList[B] = this match {
    case Nope => Cons(a, Nope)
    case Cons(h, t) => Cons(a, t)
  }

  def dropWhile(f: A => Boolean): SinglyLinkedList[A] = this match {
    case Nope => Nope
    case Cons(h, t) => if (f(h)) {
      t.dropWhile(f)
    } else {
      this
    }
  }

  def foldRight[B](ident: B, f: (A, B) => B): B = {
    this match {
      case Nope => ident
      case Cons(h, t) => f(h, t.foldRight(ident, f))
    }
  }

  def append[B >: A](b: SinglyLinkedList[B]): SinglyLinkedList[B] = this match {
    case Nope => b
    case Cons(h, t) => Cons(h, t.append(b))
  }

  def map[T >: A, R](fun: T => R): SinglyLinkedList[R] = {
    foldLeft[SinglyLinkedList[R]](Nope, (acc: SinglyLinkedList[R], a: A) => acc.append(Cons(fun(a), Nope)))
  }

  def flatMap[T >: A, R](fun: T => SinglyLinkedList[R]): SinglyLinkedList[R] = {
    foldLeft[SinglyLinkedList[R]](Nope, (acc: SinglyLinkedList[R], a: A) => {
      acc.append(fun(a))
    })
  }

  def filter(fun: A => Boolean): SinglyLinkedList[A] = {
    flatMap[A, A](item => if (fun(item)) Cons(item, Nope) else Nope)
  }

  def zipWith[T >: A, R](l2: SinglyLinkedList[T], fun: (T, T) => R): SinglyLinkedList[R] = {
    Nope
  }
}

case object Nope extends SinglyLinkedList[Nothing] {
  override def head(): Nothing = { throw new NoSuchElementException() }
  override def tail(): SinglyLinkedList[Nothing] = { Nope }
}

case class Cons[+A](head: A, tail: SinglyLinkedList[A]) extends SinglyLinkedList[A]

object SinglyLinkedList {

  def apply[A](a: A*): SinglyLinkedList[A] = {
    if (a.isEmpty) {
      Nope
    } else {
      Cons(a.head, apply(a.tail: _*))
    }
  }
}
