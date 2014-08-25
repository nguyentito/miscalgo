mod kmp;
mod palindromes;

fn main() {
    let search_tests = [("ana", "banana"),
                        ("abra", "abracadabra"),
                        ("bar", "foobarbazquux"),
                        ("foo", "fobarbazquux")];
    for &(needle, haystack) in search_tests.iter() {
        println!("occurrences of \"{}\" in \"{}\": {}",
                 needle, haystack,
                 kmp::string_search_kmp(needle.as_bytes(), haystack.as_bytes()));
    }
}

