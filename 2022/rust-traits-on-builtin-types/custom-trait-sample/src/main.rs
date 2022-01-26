trait Magic {
    fn magic_num(&self) -> usize;
}

struct Foobar {
    name: String,
}

impl Magic for Foobar {
    fn magic_num(&self) -> usize {
        return if self.name.len() == 0 { 2 } else { 33 };
    }
}

impl Magic for bool {
    fn magic_num(&self) -> usize {
        return if *self { 3 } else { 54 };
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
}
