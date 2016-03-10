

pub enum LinkedList<'a, T: 'a + Sized> {
    Nope,
    Cons(T, &'a LinkedList<'a, T>)
}

impl <'a, T: Sized> LinkedList<'a, T> {

    pub fn new<S: Sized>() -> LinkedList<'a, S> {
        LinkedList::Nope
    }

    pub fn is_empty(&self) -> bool {
        match *self {
            LinkedList::Nope => true,
            LinkedList::Cons(_, _) => false
        }
    }
}

mod test {
    use super::LinkedList;

    #[test]
    fn an_empty_linked_list_can_be_created() {
        let empty_list: LinkedList<u32> = LinkedList::<u32>::new();
        assert!(empty_list.is_empty());
    }
}
