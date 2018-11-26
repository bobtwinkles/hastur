//! The "pattern engine" handles matching file names against lists of patterns

/// A matcher for file names, compatible with Makefiles
#[derive(Clone,Default,Debug)]
pub struct PatternEngine<T> {
    patterns: Vec<String>,
    mappings: Vec<T>,
}

impl<T> PatternEngine<T> {
    /// Get the list of values associated with the patterns matching the
    /// provided query
    pub fn matches(&self, query: &str) -> Vec<T> {
        debug!("Attempting pattern search on {:?}", query);
        unimplemented!("Pattern searching")
    }
}
