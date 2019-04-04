//! Implementation of parsing file sequences.
//! This includes the implementation of `parse_file_seq` and supporting
//! functionality, including glob expansion
use crate::evaluated::BlockSpan;
use crate::parsers::makefile_whitespace;
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct FileSeqParseOptions {
    /// When true, we will strip leading `./`s from the beginning of all file names
    pub strip_leading_dotslash: bool,
    /// When true, we will check file names for archive references
    pub check_ar: bool,
    /// Globs should be expanded
    pub do_glob: bool,
    /// Only return files that actually exist
    pub exist_only: bool,
    /// Which directory to treat as CWD
    pub cwd: PathBuf,
    /// What extra characters to stop on
    pub extra_stopchars: &'static str,
}

impl Default for FileSeqParseOptions {
    fn default() -> Self {
        // XXX: we should default strip_leading_dotslash, check_ar, and do_glob to true
        Self {
            strip_leading_dotslash: false,
            check_ar: false,
            do_glob: false,
            exist_only: false,
            cwd: std::env::current_dir().expect("Failed to get current directory"),
            extra_stopchars: ""
        }
    }
}

pub(crate) fn parse_file_seq<'a>(i: BlockSpan<'a>, options: FileSeqParseOptions) -> Vec<String> {
    use nom::Slice;
    // Implementation follows parse_file_seq in read.c from GNU Make 4.2.1
    assert!(!options.check_ar);
    assert!(!options.do_glob);
    assert!(!options.exist_only);

    let mut output_names = Vec::new();

    // TODO: finish implementing this properly. Right now it only handles
    // space-separated file names, not globs or archives
    let (mut remaining_content, _) =
        makefile_whitespace(i).expect("whitespace search should never fail");

    debug!("Parsing file sequence from {:?}", remaining_content.into_string());

    while remaining_content.len() > 0 {
        match crate::parsers::makefile_take_until_unquote(remaining_content, |c| {
            c.is_whitespace() || c == '\0' || options.extra_stopchars.find(c).is_some()
        }) {
            (pre, Some((_, post))) => {
                if pre.len() > 0 {
                    output_names.push(clean_match(pre.span(), &options));
                }
                remaining_content = post;
            }
            (pre, None) => {
                remaining_content = remaining_content.slice(remaining_content.len()..);
                if pre.len() > 0 {
                    output_names.push(clean_match(pre.span(), &options));
                }
            }
        }
    }

    output_names
}

fn clean_match<'a>(mut i: BlockSpan<'a>, options: &FileSeqParseOptions) -> String {
    debug!("Cleaning file name match {:?}", i.into_string());
    if options.strip_leading_dotslash {
        i = match tag!(i, "./") {
            Ok((new_i, _)) => new_i,
            Err(_) => i,
        }
    }

    i.into_string()
}
