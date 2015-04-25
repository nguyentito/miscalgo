extern crate quickcheck;

use quickcheck::quickcheck;
use palindromes::is_palindrome;

mod kmp;
mod palindromes;


// QuickCheck properties for Manacher's algorithm

fn prop_result_is_palindrome(vec: Vec<u8>) -> bool {
    // the empty vector is not supported? TODO fix this
    if vec.len() == 0 { return true }
    let (start, end) = palindromes::lps_manacher(vec.as_slice());
    is_palindrome(vec.slice(start,end))
}

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

    println!("");

    // println!("{}", is_palindrome("foofoo".as_bytes()));
    // println!("{}", is_palindrome("foof".as_bytes()));
    // println!("{}", is_palindrome("fooof".as_bytes()));
    println!("{}", palindromes::lps_manacher([0u,1,0]));
    println!("{}", palindromes::lps_manacher([0u,1,0,1]));
    quickcheck(prop_result_is_palindrome);
}

