/// Assert that the provided `Result<T, E>` is the Ok variant
#[macro_export]
macro_rules! assert_ok {
    ($r:expr) => {{
        let r = $r;
        match r {
            Ok(v) => v,
            Err(e) => panic!("Unexpected error {:?}", e),
        }
    }};
}

/// Assert that the provided `Result<T, E>` is the Error variant
#[macro_export]
macro_rules! assert_err {
    ($r:expr) => {{
        let r = $r;
        match r {
            Ok(v) => panic!("Unexpected success {:?}", v),
            Err(e) => e,
        }
    }};
}

/// Assert that a `nom` error contains a specific error
#[macro_export]
macro_rules! assert_err_contains {
    ($err:expr, $e:expr) => {{
        let context =
            crate::parsers::test::error_context($err).expect("There should be an error context");
        let errors = std::dbg!(nom::error_to_list(&context));
        assert!(error_list_contains(&errors, ErrorKind::Custom($e)));
    }};
}

/// Assert that the provided `BlockSpan` is empty
#[macro_export]
macro_rules! assert_complete {
    ($b:expr) => {{
        let b = $b;
        assert_eq!(b.segments().next(), None);
    }};
}

/// Assert that the provided `BlockSpan` has the expected contents
#[macro_export]
macro_rules! assert_segments_eq {
    ($s:expr, $contents:expr) => {{
        use crate::source_location::LocatedStr;
        let s = $s;
        let contents = $contents;

        for (ref segment, (content, loc)) in s.segments().zip(contents.iter()) {
            assert_eq!(segment, &LocatedStr::new(loc.clone().into(), content));
        }

        assert_eq!(s.segments().count(), contents.len());
    }};
}
