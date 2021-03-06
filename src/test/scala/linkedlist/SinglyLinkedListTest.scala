package linkedlist

import org.scalatest.{Matchers, path}


class SinglyLinkedListTest extends path.FunSpec with Matchers {

  describe("a singly linked list (exercise 3)") {

    it("can be constructed by calling SinglyLinkedList(args*)") {

      val result = SinglyLinkedList(1, 2, 3)
      assert(result == Cons(1, Cons(2, Cons(3, Nope))))
    }

    describe("getting the tail of the list (exercise 3.2)") {

      val myList = SinglyLinkedList(1, 2, 3, 4)

      it ("returns the all elements but the first when tail() is called") {
        assert(SinglyLinkedList(2, 3, 4) == myList.tail())
      }
    }

    describe("an empty SinglyLinkedList") {

      it("throws a NoSuchElementException when head is called") {
        intercept[NoSuchElementException] {
          linkedlist.Nope.head()
        }
      }

      it("returns itself when tail() is called") {
        linkedlist.Nope.tail().should(be(linkedlist.Nope))
      }
    }

    describe("replacing the head of the list") {
      val start = SinglyLinkedList('a', 'b', 'c')

      it("should return a new list without modifying the old one") {
        val result = start.replaceHead('j')
        assert(result == SinglyLinkedList('j', 'b', 'c'))
        assert(start == SinglyLinkedList('a', 'b', 'c'))
      }

      it("should turn a Nope into a Cons") {
        val empty: SinglyLinkedList[String] = Nope
        val result = empty.replaceHead("myString")
        assert(result == SinglyLinkedList("myString"))
      }
    }

    describe("dropWhile") {
      val myList = SinglyLinkedList(1, 2, 3, 4)

      it("returns the same list if the function returns false for the first element") {
        val result = myList.dropWhile(it => false)
        assert(result == myList)
        assert(myList == SinglyLinkedList(1, 2, 3, 4))
      }

      it("removes all elements if the function returns true for all elements") {
        val result = myList.dropWhile(it => true)
        assert(result == Nope)
        assert(myList == SinglyLinkedList(1, 2, 3, 4))
      }

      it("removes elements until the function returns true") {
        val result = myList.dropWhile(it => it < 3)
        assert(result == SinglyLinkedList(3, 4))
        assert(myList == SinglyLinkedList(1, 2, 3, 4))
      }
    }

    describe("foldRight") {
      val intList = SinglyLinkedList(1, 2, 3, 4, 5 )

      it("sums the list using foldRight") {
        assert(15 == intList.foldRight[Int](0, (a, b) => a + b ))
      }

      it("can get the length of a list using foldRight") {
        assert(5 == intList.foldRight[Int](0, (a, b) => b + 1))
      }
    }

    describe("foldLeft") {
      val startList = SinglyLinkedList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      it("calculates the size of the list using foldLeft") {
        assert(10 == startList.foldLeft[Int](0, (b, a) => b + 1))
      }

      it("sums the list using foldLeft") {
        val expectedSum = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
        assert(expectedSum == startList.foldLeft[Int](0, _ + _))
      }
    }

    describe("concatenating two lists") {
      val l1 = SinglyLinkedList(1, 2, 3)
      val l2 = SinglyLinkedList(4, 5, 6)

      val joined = l1.append(l2)

      it("joins the two lists together"){
        assert(joined == SinglyLinkedList(1, 2, 3, 4, 5, 6))
      }

      it("does not modify either of the original lists") {
        assert(l1 == SinglyLinkedList(1, 2, 3))
        assert(l2 == SinglyLinkedList(4, 5, 6))
      }
    }
  }

  describe("mapping list elements"){
    val start = SinglyLinkedList(1, 2, 3)

    it("changes elements from one type to another") {
      val result = start.map((i: Int) => i.toString)
      assert(result == SinglyLinkedList("1", "2", "3"))
    }
  }

  describe("filtering list elements") {
    val numbers = SinglyLinkedList(1, 2, 3, 4, 5, 6, 7)

    it("should filter the list to only include even numbers") {
      val evens = numbers.filter(i => i % 2 == 0)
      assert(evens == SinglyLinkedList(2, 4, 6))
      assert(numbers == SinglyLinkedList(1, 2, 3, 4, 5, 6, 7))
    }
  }

  describe("flatMap"){
    val start = SinglyLinkedList("a", "b", "c")

    it("should return a flat list"){
      val result = start.flatMap[String, String]((letter: String) => SinglyLinkedList(letter, letter))
      assert(result == SinglyLinkedList("a", "a", "b", "b", "c", "c"))
    }
  }

  describe("zipWith") {

    it("zips two lists of integers") {
      val l1 = SinglyLinkedList(2, 4, 6)
      val l2 = SinglyLinkedList(8, 6, 4)

      val result = l1.zipWith[Int, Int](l2, (i1, i2) => i1 + i2)
      assert(result == SinglyLinkedList(10, 10, 10))
    }
  }

}
