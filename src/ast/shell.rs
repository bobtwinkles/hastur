//! Implement functions that interact with the underlying operating system like
//! the shell would.

use crate::evaluated::{Block, BlockSpan};
use std::sync::Arc;

/// The most generic shell function: dump garbage to the shell
pub(super) fn shell(input: BlockSpan) -> Arc<Block> {
    Block::new(Default::default(), vec![])
}
