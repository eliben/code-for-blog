// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
trait Magic {
    fn magic_num(&self) -> usize;
}

struct Foobar {
    name: String,
}

impl Magic for Foobar {
    fn magic_num(&self) -> usize {
        if self.name.is_empty() {
            2
        } else {
            33
        }
    }
}

impl Magic for bool {
    fn magic_num(&self) -> usize {
        if *self {
            3
        } else {
            54
        }
    }
}

impl<T> Magic for Vec<T> {
    fn magic_num(&self) -> usize {
        if self.is_empty() {
            10
        } else {
            5
        }
    }
}

fn report_magic<M: Magic>(v: &M) {
    println!("magic num: {}", v.magic_num());
}

fn main() {
    let fb = Foobar {
        name: "Joe".to_string(),
    };

    report_magic(&fb);

    let tv = false;
    report_magic(&tv);

    println!("{}", true.magic_num());

    let vv = vec![1, 2, 3];
    report_magic(&vv);
}
