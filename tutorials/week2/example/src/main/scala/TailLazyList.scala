import java.util.NoSuchElementException

/**
  * lazy implement indicates that lazy tail is lazy
  * lazy head is not lazy
  * isEmpty is not lazy
  * */
trait TailLazyList[+A]

object TailLazyList:
  def cons[A](hd: A, tl: => TailLazyList[A]) = new TailLazyList[A] { // for lazyList, tl using by-name parameters
    def isEmpty = false
    def head = hd
    def tail = tl
    override def toString() = "LazyList(" + hd + ", ?)"
  }
  val empty = new TailLazyList[Nothing] {
    def isEmpty = true
    def head = throw NoSuchElementException("head of empty lazylist")
    def tail = throw NoSuchElementException("tail of empty lazylist")
    override def toString() = "LazyList()"
  }
