mod kmp;
mod palindromes;

fn main() {
    let search_tests = [("ana", "banana"),
                        ("abracadabra", "abracadabracadabra"),
                        ("bar", "foobarbazquux"),
                        ("foo", "fobarbazquux"),
                        ("foo", "foofoobar")];
    for &(needle, haystack) in search_tests.iter() {
        println!("occurrences of \"{}\" in \"{}\": {}",
                 needle, haystack,
                 kmp::string_search(needle.as_bytes(), haystack.as_bytes()));
    }
    println!("failure array for \"abracadabra\": {}",
             kmp::build_automaton("abracadabra".as_bytes()));
}

