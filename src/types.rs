//! Shorthand for some annoying-to-write types

pub type Set<T> = im::HashSet<T, fxhash::FxBuildHasher>;
pub type Map<K, V> = im::HashMap<K, V, fxhash::FxBuildHasher>;
