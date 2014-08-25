// Knuth-Morris-Pratt algorithm
// cf. Jeff Erickson's lecture notes on Algorithms
//
// The algorithm is based on an automaton with n+2 states
// (n = pattern/needle length):
// * an initial state ($ in Erickson, represented here by uint::MAX
// * n+1 states numbered from 0 to n
//   state k is reached after succesfully consuming a prefix of length k,
//   i.e. after the (k-1)th character of the needle (in 0-based indexing,
//   when k >= 1) has been matched against the haystack.
// Note that the state with label n is the final state, indicating a
// successful match; it is denoted "!" in Erickson.
//
// Transitions:
// * i -(ith character)-> i+1   -- implicit
// * i -(epsilon)-> $ or j < i  -- stored in "failure array"
// * $ -(any character)-> 0     -- implicit
// There is exactly 1 epsilon-move outgoing from all states except
// $. That includes the final state, unlike in Erickson, so that we
// may find all matches, not just one). We store them in an array of
// length n+1.


static initial_state: uint = std::uint::MAX;
#[inline]
fn successor_state(j: uint) {
    if j == initial_state { 0 } else { j + 1 };
} // hopefully this just compiles into a +1 with overflow

// computes the "failure array" with dynamic programming
fn kmp_build_automaton<T:Eq>(p: &[T}) -> Vec<uint> {
    let num_states = p.len() + 1; // excluding the unrepresented initial state
    let mut f = Vec::with_capacity(num_states);

    f.push(initial_state); // we assume p.len() > 0
    let mut j = 0;
    for i in range(1, p.len()) {
        // invariant: 0 <= j <= p.len() - 1 at this point of the loop
        f.push(if p[i] == p[j] { f[j] } else { j }); 
        while j != initial_state && p[i] != p[j] {
            j = f[j];
        }
        j = successor_state(j);
    }
    f.push(j);

    return f;
}

fn kmp_run_automaton<T:Eq>(needle: &[T], automaton: &[uint], haystack: &[T]) {
    let mut i = 0; // current character in haystack
    let mut j = 0; // automaton state
    let l = needle.len();
    let mut occ = Vec::new();

    while i < haystack.len() { 
        if j == l { // final state reached: occurrence found
            // push index of the start of the match for convenience
            // i is the index of the end
            occ.push(i - l + 1);
            j = automaton[j];
        } else if j == initial_state || haystack[i] == needle[j] {
            i += 1;
            j = successor_state(j);
        } else { // intermediate state, mismatch
            j = automaton[j];
        }
    }
    return occ;
}


// package everything into a single function for one-shot use
// you can also preprocess once and search for the same pattern in
// different strings using the two functions above
fn string_search_kmp<T:Eq>(needle: &[T], haystack: &[T]) -> Vec<uint> {
    let aut = kmp_build_automaton(needle);
    kmp_run_automaton(needle, aut.as_slice(), haystack)
}





