package quickcheck

import org.scalacheck.*
import Arbitrary.*
//import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
//  lazy val genHeap: Gen[H] = Gen.oneOf(
//    Gen.const(empty),
//    for
//      k <- arbitrary[Int]
//      m <- Gen.oneOf(Gen.const(empty), genHeap)
//    yield insert(k, m)
//  )

  lazy val genHeap: Gen[H] = {
    for
      k <- arbitrary[Int]
      m <- Gen.oneOf(Gen.const(empty), genHeap)
    yield insert(k, m)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
//  given Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll {(a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of 2-elements heap is minimum of 2 elements") = forAll {(a: Int, b: Int) =>
    val lesser = if(a > b) b else a
    val greater = if(a > b) a else b
    val h = insert(a, insert(b, empty))
    (findMin(h) == lesser) && (findMin(deleteMin(h)) == greater)
  }

//  property("min2") = forAll {(a: Int) =>
//    val h = insert(a, empty)
//    findMin(insert(a, deleteMin(h))) == a
//  }

  property("deleteMin from 1-element heap return empty") = forAll {(a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("delete min 2 times of 2-elements heap return empty") = forAll {(a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    deleteMin(deleteMin(h)) == empty
  }

  property("min of meld heap is min of each min of heaps") = forAll {(h1: H, h2: H) =>
    val comh = meld(h1, h2)
    findMin(comh) == Math.min(findMin(h1), findMin(h2))
  }

  property("melding 3 times and delete 3 mins, next mins are equals") = forAll {(h: H) =>
    val h3 = meld(h, meld(h, h))
    val h1 = deleteMin(deleteMin(deleteMin(h3)))
    val h2 = deleteMin(h)
    isEmpty(h2) || findMin(h1) == findMin(h2)
  }

  def elemSeq(h: H, acc: Seq[Int]): Seq[Int] = {
    if(isEmpty(h)) acc
    else
      elemSeq(deleteMin(h), acc ++ Seq(findMin(h)))
  }

  property("sorted sequences when deleting min") = forAll {(h: H) =>
    val res = elemSeq(h, Seq())
    res == res.sorted
  }

  property("left meld empty return same heap") = forAll {(h: H) =>
    meld(empty, h) == h
  }

  property("right meld empty return same heap") = forAll {(h: H) =>
    meld(h, empty) == h
  }

//
//  property("meld") = forAll {(h1: H, h2: H) =>
//    val m1 = if(isEmpty(h1)) 0 else findMin(h1)
//    val m2 = if(isEmpty(h2)) 0 else findMin(h2)
//    if((m1 != 0) && (m2 != 0)){
//      if(m1 < m2) findMin(meld(h1, h2)) == m1
//      else findMin(meld(h1, h2)) == m2
//    }
//    else if(m1 == 0) findMin(meld(h1, h2)) == m2
//    else findMin(meld(h1, h2)) == m1
//  }


  property("test") = {
    val l = List(4, 2, 1, 3)
    val h = insert(l.head, insert(l.tail.head, insert(l.tail.tail.head, insert(l.tail.tail.tail.head, empty))))
    (findMin(h) == 1) && (findMin(deleteMin(h)) == 2) && (findMin(deleteMin(deleteMin(h))) == 3) &&
      (findMin(deleteMin(deleteMin(deleteMin(h)))) == 4)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }



