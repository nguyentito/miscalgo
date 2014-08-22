fn main() {
    println!("{}", "hello world");
}

// Direct translation of code from
// http://algs4.cs.princeton.edu/53substring/Manacher.java.html
// (only differences: using an enum & parametric polymorphism)

fn lps_manacher<T:Clone+Eq>(arr: &[T]) -> (uint, uint) {

    // TODO: once we get DST, replace Vec<T> with ~[T]
    // since we don't need to grow the array
    #[deriving(PartialEq, Eq)]
    enum EltSpace<T> {
        Start, End, Mid,
        Symb(T),
    }

    fn preprocess<T:Clone>(arr: &[T]) -> Vec<EltSpace<T>> {
        let n = arr.len();
        let mut vec = Vec::with_capacity(2*n+3);
        vec.push(Start);
        for i in range(0,n) {
            vec.push(Symb(arr[i].clone()));
            vec.push(Mid);
        }
        vec.push(End);
        vec
    }

    let vec = preprocess(arr);
    let n = vec.len();

    let mut lps = Vec::from_elem(0, n);
    let mut center = 0;
    let mut right = 0;

    for i in range(1, n-1) {
        let mirror = 2*center - i;
        
        if right > i {
            *lps.get_mut(i) = std::cmp::min(right - i, lps[mirror]);
        }

        while vec[i + 1 + lps[i]] == vec[i - 1 - lps[i]] {
            // wait what? this doesn't work?
//            lps[i] += 1;
            *lps.get_mut(i) += 1;
        }

        if i + lps[i] > right {
            center = i;
            right = i + lps[i];
        }
    }

    // Not idiomatic Rust!
    let mut max_len = 0;
    let mut max_center = 0;
    for i in range(1, n-1) {
        if lps[i] > max_len {
            max_len = lps[i];
            max_center = i;
        }
    }
    
    ((max_center - 1 - max_len) / 2, (max_center - 1 + max_len) / 2)
}

