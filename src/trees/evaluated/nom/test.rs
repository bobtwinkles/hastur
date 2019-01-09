use crate::trees::evaluated::EvaluatedTreeSpan;
use crate::trees::evaluated::nodes::{Root, Constant, Concat};
use super::*;

/*
fn create_test(constants: &[&str]) -> EvaluatedTreeSpan {
    let parent = Root {
        
    };
    let child_nodes = constants.iter().map(|x| {
        let contents = Constant {
            
        }
    });
    EvaluatedTreeSpan {
        parent,
    }
}

mod revalidate {
    use super::*;

    #[test]
    fn no_work() {
        let under_test = makefile_line(create_span("ab\\\nbc\\\nd"), ParserCompliance::GNU, false)
            .unwrap()
            .1;
        let mut under_test = under_test.as_span();
        under_test.offset = 1;
        under_test.revalidate_offset();

        assert_eq!(under_test.parent[0], leftover_span("ab", 0, 1),);
    }

    #[test]
    fn drops() {
        let under_test = makefile_line(create_span("ab\\\nbc\\\nd"), ParserCompliance::GNU, false)
            .unwrap()
            .1;
        let mut under_test = under_test.as_span();
        under_test.offset = 3;
        under_test.revalidate_offset();

        assert_eq!(under_test.parent.len(), 3);
        assert_eq!(under_test.parent[0], leftover_span("bc", 4, 2),);
    }
}

mod slice {
    use super::*;
    #[test]
    fn no_skip() {
        let under_test = makefile_line(create_span("a\\\nbc\\\nd"), ParserCompliance::GNU, false)
            .unwrap()
            .1;
        let under_test = under_test.as_span();
        let res = under_test.slice(0..1);

        assert_eq!(res.parent[0], leftover_span("a", 0, 1));
        assert_eq!(res.offset, 0);
        assert_eq!(res.length, 1);
    }

    #[test]
    fn skip_then_contained() {
        let under_test = makefile_line(create_span("a\\\nbc\\\nd"), ParserCompliance::GNU, false)
            .unwrap()
            .1;
        let under_test = under_test.as_span();
        let res = under_test.slice(2..4);

        assert_eq!(res.parent[0], leftover_span("bc", 3, 2));
        assert_eq!(res.offset, 0);
        assert_eq!(res.length, 2);
    }

    #[test]
    fn skip_then_split() {
        let under_test = makefile_line(create_span("a\\\nbc\\\nd"), ParserCompliance::GNU, false)
            .unwrap()
            .1;
        let under_test = under_test.as_span();
        let res = under_test.slice(2..5);

        assert_eq!(res.parent[0], leftover_span("bc", 3, 2));
        assert_eq!(res.parent[1], leftover_span(" ", 7, 3));
        assert_eq!(res.offset, 0);
        assert_eq!(res.length, 3);
    }

    #[test]
    fn skip_then_rest() {
        let under_test = makefile_line(create_span("a\\\nbc\\\nd"), ParserCompliance::GNU, false)
            .unwrap()
            .1;
        let under_test = under_test.as_span();
        let res = under_test.slice(3..);

        assert_eq!(res.flatten(), String::from("c d"));
    }

    #[test]
    fn skip_then_rest_offset() {
        let under_test = makefile_line(create_span("ab\\\nbc\\\nd"), ParserCompliance::GNU, false)
            .unwrap()
            .1;
        let mut under_test = under_test.as_span();
        under_test.offset += 1;
        under_test.revalidate_offset();
        let res = under_test.slice(3..);

        assert_eq!(res.flatten(), String::from("c d"));
    }

    #[test]
    fn range_to_simple() {
        let under_test = simple_line("a\\\nbcdefg");
        let under_test = under_test.as_span();

        assert_eq!(under_test.slice(..1).flatten(), "a");
        assert_eq!(under_test.slice(..3).flatten(), "a b");
    }
}

mod split_at {
    use super::*;

    #[test]
    fn simple() {
        let under_test = simple_line("a\\\n1\\\nc");
        let under_test = under_test.as_span();
        let res = under_test.split_at_position(|c| c == '1');
        let res = res.ok().unwrap();

        assert_eq!(
            (res.0.flatten(), res.1.flatten()),
            (String::from("1 c"), String::from("a "))
        );
    }

    #[test]
    fn simple1() {
        let under_test = simple_line("a\\\n1\\\nc");
        let under_test = under_test.as_span();
        let res = under_test.split_at_position1(|c| c == '1', ErrorKind::Char);
        let res = res.ok().unwrap();

        assert_eq!(
            (res.0.flatten(), res.1.flatten()),
            (String::from("1 c"), String::from("a "))
        );
    }

    #[test]
    fn atleast_fails() {
        let under_test = simple_line("a\\\n1\\\nc");
        let under_test = under_test.as_span();
        let res = under_test.split_at_position1(|c| c == 'a', ErrorKind::Char);

        assert!(res.is_err());
    }
}

mod compare {
    use super::*;

    #[test]
    fn simple_eq() {
        let under_test = simple_line("asdf");
        let under_test = under_test.as_span();

        assert_eq!(under_test.compare("as"), CompareResult::Ok)
    }

    #[test]
    fn simple_same() {
        let under_test = simple_line("asdf");
        let under_test = under_test.as_span();

        assert_eq!(under_test.compare("asdf"), CompareResult::Ok)
    }

    #[test]
    fn simple_incomplete() {
        let under_test = simple_line("asdf");
        let under_test = under_test.as_span();

        assert_eq!(under_test.compare("asdfjkl"), CompareResult::Incomplete)
    }

    #[test]
    fn simple_error() {
        let under_test = simple_line("asdf");
        let under_test = under_test.as_span();

        assert_eq!(under_test.compare("asdq"), CompareResult::Error)
    }

    #[test]
    fn no_case_eq() {
        let under_test = simple_line("asdf");
        let under_test = under_test.as_span();

        assert_eq!(under_test.compare_no_case("aS"), CompareResult::Ok)
    }

    #[test]
    fn no_caes_same() {
        let under_test = simple_line("asdf");
        let under_test = under_test.as_span();

        assert_eq!(under_test.compare_no_case("aSdf"), CompareResult::Ok)
    }

    #[test]
    fn no_case_incomplete() {
        let under_test = simple_line("asdf");
        let under_test = under_test.as_span();

        assert_eq!(
            under_test.compare_no_case("asDfjkl"),
            CompareResult::Incomplete
        )
    }

    #[test]
    fn no_case_error() {
        let under_test = simple_line("asdf");
        let under_test = under_test.as_span();

        assert_eq!(under_test.compare_no_case("asDq"), CompareResult::Error)
    }
}

mod input_iter {
    use super::*;

    #[test]
    fn elements_iter() {
        let under_test = makefile_line(create_span("a\\\nbc\\\nd"), ParserCompliance::GNU, false)
            .unwrap()
            .1;
        let under_test = under_test.as_span();
        let mut iter = under_test.iter_elements();

        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.next(), Some(' '));
        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.next(), Some('c'));
        assert_eq!(iter.next(), Some(' '));
        assert_eq!(iter.next(), Some('d'));
    }

    #[test]
    fn elments_iter_offset() {
        let under_test = simple_line("ab\\\nb");
        let mut under_test = under_test.as_span();
        under_test.offset = 1;
        under_test.revalidate_offset();
        let mut iter = under_test.iter_elements();

        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.next(), Some(' '));
        assert_eq!(iter.next(), Some('b'));
    }
}

*/
