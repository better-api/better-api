use crate::path::{PathArena, PathPart};

#[test]
fn compare_simple() {
    let mut arena = PathArena::new();

    // All empty
    let left = arena.insert(None, PathPart::Empty);
    let right = arena.insert(None, PathPart::Empty);
    assert!(arena.get(left) == arena.get(right));

    // Left empty
    let left = arena.insert(None, PathPart::Empty);
    let right = arena.insert(None, PathPart::Segment("/foo"));
    assert!(arena.get(left) < arena.get(right));

    // Right empty
    let left = arena.insert(None, PathPart::Segment("/foo"));
    let right = arena.insert(None, PathPart::Empty);
    assert!(arena.get(left) > arena.get(right));

    // Some params
    let left = arena.insert(None, PathPart::Segment("/foo/{bar}"));
    let right = arena.insert(None, PathPart::Segment("/foo/{other_bar}"));
    assert!(arena.get(left) == arena.get(right));

    // Some more params
    let left = arena.insert(None, PathPart::Segment("/{bar}"));
    let right = arena.insert(None, PathPart::Segment("/foo"));
    assert!(arena.get(left) > arena.get(right));

    // Even more params
    let left = arena.insert(None, PathPart::Segment("/{bar}"));
    let right = arena.insert(None, PathPart::Segment("/{foo}/{bar}"));
    assert!(arena.get(left) < arena.get(right));
}

#[test]
fn complex_equality() {
    let mut arena = PathArena::new();

    let left = arena.insert(None, PathPart::Segment("/foo/{bar}/baz/{xyz}"));

    let r1 = arena.insert(None, PathPart::Empty);
    let r2 = arena.insert(Some(r1), PathPart::Segment("/foo"));
    let r3 = arena.insert(Some(r2), PathPart::Empty);
    let r4 = arena.insert(Some(r3), PathPart::Empty);
    let r5 = arena.insert(Some(r4), PathPart::Segment("/{param1}/baz"));
    let r6 = arena.insert(Some(r5), PathPart::Segment("/{param2}"));
    let r7 = arena.insert(Some(r6), PathPart::Empty);
    let right = arena.insert(Some(r7), PathPart::Empty);

    assert!(arena.get(left) == arena.get(right));
    assert!(arena.get(right) == arena.get(left));
    assert!(arena.get(right) == arena.get(right));
    assert!(arena.get(left) == arena.get(left));
}

#[test]
fn complex_inequality() {
    let mut arena = PathArena::new();

    let left = arena.insert(None, PathPart::Segment("/foo/{bar}/baz/{xyz}"));

    let r1 = arena.insert(None, PathPart::Empty);
    let r2 = arena.insert(Some(r1), PathPart::Segment("/foos"));
    let r3 = arena.insert(Some(r2), PathPart::Empty);
    let r4 = arena.insert(Some(r3), PathPart::Empty);
    let r5 = arena.insert(Some(r4), PathPart::Segment("/{param1}/baz"));
    let r6 = arena.insert(Some(r5), PathPart::Segment("/{param2}"));
    let r7 = arena.insert(Some(r6), PathPart::Empty);
    let right = arena.insert(Some(r7), PathPart::Empty);

    assert!(arena.get(left) < arena.get(right));
    assert!(arena.get(right) > arena.get(left));
}

#[test]
fn parameter_vs_static_ordering() {
    let mut arena = PathArena::new();

    // Static segment comes before parameter in same position
    let static_path = arena.insert(None, PathPart::Segment("/foo/bar"));
    let param_path = arena.insert(None, PathPart::Segment("/foo/{id}"));
    assert!(arena.get(static_path) < arena.get(param_path));
    assert!(arena.get(param_path) > arena.get(static_path));

    // Static vs param with different prefixes
    let p1 = arena.insert(None, PathPart::Segment("/foo"));
    let static_path2 = arena.insert(Some(p1), PathPart::Segment("/bar"));
    let p2 = arena.insert(None, PathPart::Segment("/foo"));
    let param_path2 = arena.insert(Some(p2), PathPart::Segment("/{id}"));
    assert!(arena.get(static_path2) < arena.get(param_path2));
}

#[test]
fn lexicographic_ordering() {
    let mut arena = PathArena::new();

    // Simple lexicographic ordering
    let aaa = arena.insert(None, PathPart::Segment("/aaa"));
    let zzz = arena.insert(None, PathPart::Segment("/zzz"));
    assert!(arena.get(aaa) < arena.get(zzz));

    // Prefix vs longer string
    let foo = arena.insert(None, PathPart::Segment("/foo"));
    let foobar = arena.insert(None, PathPart::Segment("/foobar"));
    assert!(arena.get(foo) < arena.get(foobar));

    // Multi-segment lexicographic ordering
    let p1 = arena.insert(None, PathPart::Segment("/foo"));
    let foo_bar = arena.insert(Some(p1), PathPart::Segment("/bar"));
    let p2 = arena.insert(None, PathPart::Segment("/foo"));
    let foo_baz = arena.insert(Some(p2), PathPart::Segment("/baz"));
    assert!(arena.get(foo_bar) < arena.get(foo_baz));
}

#[test]
fn multiple_consecutive_params() {
    let mut arena = PathArena::new();

    // Multiple params with different names should be equal
    let left = arena.insert(None, PathPart::Segment("/{a}/{b}/{c}"));
    let right = arena.insert(None, PathPart::Segment("/{x}/{y}/{z}"));
    assert!(arena.get(left) == arena.get(right));
    assert!(arena.get(right) == arena.get(left));

    // With prefixes
    let p1 = arena.insert(None, PathPart::Segment("/foo"));
    let left2 = arena.insert(Some(p1), PathPart::Segment("/{a}/{b}"));
    let p2 = arena.insert(None, PathPart::Segment("/foo"));
    let right2 = arena.insert(Some(p2), PathPart::Segment("/{x}/{y}"));
    assert!(arena.get(left2) == arena.get(right2));
}

#[test]
fn transitivity() {
    let mut arena = PathArena::new();

    // Transitivity with simple paths
    let a = arena.insert(None, PathPart::Segment("/aaa"));
    let b = arena.insert(None, PathPart::Segment("/bbb"));
    let c = arena.insert(None, PathPart::Segment("/ccc"));

    assert!(arena.get(a) < arena.get(b));
    assert!(arena.get(b) < arena.get(c));
    assert!(arena.get(a) < arena.get(c));

    // Transitivity with static < param ordering
    let static_seg = arena.insert(None, PathPart::Segment("/foo/bar"));
    let param_seg = arena.insert(None, PathPart::Segment("/foo/{id}"));
    let longer_param = arena.insert(None, PathPart::Segment("/foo/{id}/extra"));

    assert!(arena.get(static_seg) < arena.get(param_seg));
    assert!(arena.get(param_seg) < arena.get(longer_param));
    assert!(arena.get(static_seg) < arena.get(longer_param));
}

// Helper function to compute hash of a Path
fn hash_path(path: &crate::path::Path) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    path.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn hash_empty_paths() {
    let mut arena = PathArena::new();

    let empty1 = arena.insert(None, PathPart::Empty);
    let empty2 = arena.insert(None, PathPart::Empty);

    assert_eq!(hash_path(&arena.get(empty1)), hash_path(&arena.get(empty2)));
}

#[test]
fn hash_basic_segments() {
    let mut arena = PathArena::new();

    let path1 = arena.insert(None, PathPart::Segment("/foo"));
    let path2 = arena.insert(None, PathPart::Segment("/foo"));
    let path3 = arena.insert(None, PathPart::Segment("/bar"));

    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path2)));
    assert_ne!(hash_path(&arena.get(path1)), hash_path(&arena.get(path3)));
}

#[test]
fn hash_params_with_different_names() {
    let mut arena = PathArena::new();

    // Parameters with different names should hash to the same value
    let path1 = arena.insert(None, PathPart::Segment("/{id}"));
    let path2 = arena.insert(None, PathPart::Segment("/{name}"));
    let path3 = arena.insert(None, PathPart::Segment("/{user_id}"));

    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path2)));
    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path3)));
}

#[test]
fn hash_params_in_path_segments() {
    let mut arena = PathArena::new();

    // Paths with params in different positions should have different hashes
    let path1 = arena.insert(None, PathPart::Segment("/foo/{id}"));
    let path2 = arena.insert(None, PathPart::Segment("/foo/{name}"));
    let path3 = arena.insert(None, PathPart::Segment("/bar/{id}"));

    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path2)));
    assert_ne!(hash_path(&arena.get(path1)), hash_path(&arena.get(path3)));
}

#[test]
fn hash_multiple_params() {
    let mut arena = PathArena::new();

    let path1 = arena.insert(None, PathPart::Segment("/foo/{id}/bar/{name}"));
    let path2 = arena.insert(None, PathPart::Segment("/foo/{user}/bar/{item}"));
    let path3 = arena.insert(None, PathPart::Segment("/foo/{id}/baz/{name}"));

    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path2)));
    assert_ne!(hash_path(&arena.get(path1)), hash_path(&arena.get(path3)));
}

#[test]
fn hash_empty_vs_segment() {
    let mut arena = PathArena::new();

    // Empty and Segment should have different hashes
    let empty = arena.insert(None, PathPart::Empty);
    let segment = arena.insert(None, PathPart::Segment("/foo"));

    assert_ne!(hash_path(&arena.get(empty)), hash_path(&arena.get(segment)));
}

#[test]
fn hash_partial_param_match() {
    let mut arena = PathArena::new();

    // Partial params should hash differently based on position
    let path1 = arena.insert(None, PathPart::Segment("/a{foo}"));
    let path2 = arena.insert(None, PathPart::Segment("/a{bar}"));
    let path3 = arena.insert(None, PathPart::Segment("/b{foo}"));

    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path2)));
    assert_ne!(hash_path(&arena.get(path1)), hash_path(&arena.get(path3)));
}

#[test]
fn hash_multi_segment_paths() {
    let mut arena = PathArena::new();

    // Build /foo/bar
    let p1 = arena.insert(None, PathPart::Segment("/foo"));
    let path1 = arena.insert(Some(p1), PathPart::Segment("/bar"));

    // Build /foo/bar again
    let p2 = arena.insert(None, PathPart::Segment("/foo"));
    let path2 = arena.insert(Some(p2), PathPart::Segment("/bar"));

    // Build /foo/baz
    let p3 = arena.insert(None, PathPart::Segment("/foo"));
    let path3 = arena.insert(Some(p3), PathPart::Segment("/baz"));

    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path2)));
    assert_ne!(hash_path(&arena.get(path1)), hash_path(&arena.get(path3)));
}

#[test]
fn hash_complex_paths_with_empty_parts() {
    let mut arena = PathArena::new();

    // Build /foo/{bar}/baz/{xyz}
    let left = arena.insert(None, PathPart::Segment("/foo/{bar}/baz/{xyz}"));

    // Build equivalent path with empty parts scattered in: Empty -> /foo -> Empty -> Empty -> /{param1}/baz -> /{param2} -> Empty -> Empty
    let r1 = arena.insert(None, PathPart::Empty);
    let r2 = arena.insert(Some(r1), PathPart::Segment("/foo"));
    let r3 = arena.insert(Some(r2), PathPart::Empty);
    let r4 = arena.insert(Some(r3), PathPart::Empty);
    let r5 = arena.insert(Some(r4), PathPart::Segment("/{param1}/baz"));
    let r6 = arena.insert(Some(r5), PathPart::Segment("/{param2}"));
    let r7 = arena.insert(Some(r6), PathPart::Empty);
    let right = arena.insert(Some(r7), PathPart::Empty);

    // These paths are equal, so they must hash the same
    assert!(arena.get(left) == arena.get(right));
    assert_eq!(hash_path(&arena.get(left)), hash_path(&arena.get(right)));
}

#[test]
fn hash_multiple_consecutive_params() {
    let mut arena = PathArena::new();

    // Multiple params with different names should be equal and hash the same
    let path1 = arena.insert(None, PathPart::Segment("/{a}/{b}/{c}"));
    let path2 = arena.insert(None, PathPart::Segment("/{x}/{y}/{z}"));

    assert!(arena.get(path1) == arena.get(path2));
    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path2)));

    // With prefixes
    let p1 = arena.insert(None, PathPart::Segment("/foo"));
    let path3 = arena.insert(Some(p1), PathPart::Segment("/{a}/{b}"));
    let p2 = arena.insert(None, PathPart::Segment("/foo"));
    let path4 = arena.insert(Some(p2), PathPart::Segment("/{x}/{y}"));

    assert!(arena.get(path3) == arena.get(path4));
    assert_eq!(hash_path(&arena.get(path3)), hash_path(&arena.get(path4)));
}

#[test]
fn hash_consistency_with_eq() {
    let mut arena = PathArena::new();

    // If two Paths are equal, they must have the same hash
    // Test with various combinations including empty parts, params, etc.

    // Case 1: Simple segments
    let a1 = arena.insert(None, PathPart::Segment("/foo"));
    let a2 = arena.insert(None, PathPart::Segment("/foo"));
    assert!(arena.get(a1) == arena.get(a2));
    assert_eq!(hash_path(&arena.get(a1)), hash_path(&arena.get(a2)));

    // Case 2: Parameters with different names
    let b1 = arena.insert(None, PathPart::Segment("/{id}"));
    let b2 = arena.insert(None, PathPart::Segment("/{name}"));
    assert!(arena.get(b1) == arena.get(b2));
    assert_eq!(hash_path(&arena.get(b1)), hash_path(&arena.get(b2)));

    // Case 3: Multi-segment with params
    let c1_p = arena.insert(None, PathPart::Segment("/foo"));
    let c1 = arena.insert(Some(c1_p), PathPart::Segment("/{id}"));
    let c2_p = arena.insert(None, PathPart::Segment("/foo"));
    let c2 = arena.insert(Some(c2_p), PathPart::Segment("/{user_id}"));
    assert!(arena.get(c1) == arena.get(c2));
    assert_eq!(hash_path(&arena.get(c1)), hash_path(&arena.get(c2)));

    // Case 4: With empty parts
    let d1 = arena.insert(None, PathPart::Empty);
    let d2 = arena.insert(None, PathPart::Empty);
    assert!(arena.get(d1) == arena.get(d2));
    assert_eq!(hash_path(&arena.get(d1)), hash_path(&arena.get(d2)));

    // Case 5: Empty parts as prefixes
    let e1_p = arena.insert(None, PathPart::Empty);
    let e1 = arena.insert(Some(e1_p), PathPart::Segment("/foo"));
    let e2_p = arena.insert(None, PathPart::Empty);
    let e2 = arena.insert(Some(e2_p), PathPart::Segment("/foo"));
    assert!(arena.get(e1) == arena.get(e2));
    assert_eq!(hash_path(&arena.get(e1)), hash_path(&arena.get(e2)));

    // Case 6: Complex paths with multiple params and empty parts
    let f1_p1 = arena.insert(None, PathPart::Empty);
    let f1_p2 = arena.insert(Some(f1_p1), PathPart::Segment("/foo/{a}/bar/{b}"));
    let f1 = arena.insert(Some(f1_p2), PathPart::Empty);

    let f2_p1 = arena.insert(None, PathPart::Empty);
    let f2_p2 = arena.insert(Some(f2_p1), PathPart::Segment("/foo/{x}/bar/{y}"));
    let f2 = arena.insert(Some(f2_p2), PathPart::Empty);

    assert!(arena.get(f1) == arena.get(f2));
    assert_eq!(hash_path(&arena.get(f1)), hash_path(&arena.get(f2)));
}

#[test]
fn hash_empty_param_edge_case() {
    let mut arena = PathArena::new();

    // Edge case: what if someone has {} (which is invalid but let's test hash consistency)
    let path1 = arena.insert(None, PathPart::Segment("/foo/{}"));
    let path2 = arena.insert(None, PathPart::Segment("/foo/{}"));

    assert!(arena.get(path1) == arena.get(path2));
    assert_eq!(hash_path(&arena.get(path1)), hash_path(&arena.get(path2)));
}
