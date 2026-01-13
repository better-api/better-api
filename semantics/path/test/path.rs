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
