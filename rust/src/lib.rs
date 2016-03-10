use std::rc::Rc;


#[derive(PartialEq, Debug)]
pub enum LinkedList<T: Sized> {
    Nope,
    Cons(T, Rc<LinkedList<T>>)
}

impl <T: Sized> LinkedList<T> {

    pub fn new<S: Sized>() -> LinkedList<S> {
        LinkedList::Nope
    }

    pub fn is_empty(&self) -> bool {
        match *self {
            LinkedList::Nope => true,
            LinkedList::Cons(_, _) => false
        }
    }

    pub fn len(&self) -> usize {
        self.fold_r(0usize, |agg: usize, _elem: &T| agg + 1)
    }

    pub fn fold_r<S, F>(&self, init_value: S, fun: F) -> S where F: Fn(S, &T) -> S {
        match *self {
            LinkedList::Nope => init_value,
            LinkedList::Cons(ref elem, ref tail) => {
                let acc = fun(init_value, &elem);
                tail.fold_r(acc, fun)
            }
        }
    }

    pub fn push(self, elem: T) -> LinkedList<T> {
        LinkedList::Cons(elem, Rc::new(self))
    }

    pub fn pop(self) -> Option<(T, Rc<LinkedList<T>>)> {
        match self {
            LinkedList::Nope => None,
            LinkedList::Cons(elem, tail) => {
                Some((elem, tail.clone()))
            }
        }
    }

}

mod test {
    use super::LinkedList;

    #[test]
    fn popping_from_an_empty_list_should_return_none() {
        let list: LinkedList<u32> = LinkedList::<u32>::new();
        let pop_result = list.pop();
        assert!(pop_result.is_none());
    }

    #[test]
    fn items_should_be_popped_from_the_list() {
        let list = LinkedList::<u32>::new().push(456).push(123);
        let (first_elem, tail1) = list.pop().expect("expected result of first pop to be Some");
        assert_eq!(123u32, first_elem);
        assert_eq!(1, tail1.len());
    }

    #[test]
    fn items_should_be_added_to_a_list() {
        let list = LinkedList::<u32>::new().push(123).push(456);

        assert_eq!(2, list.len());
    }

    #[test]
    fn an_empty_linked_list_can_be_created() {
        let empty_list: LinkedList<u32> = LinkedList::<u32>::new();
        assert!(empty_list.is_empty());
    }
}
