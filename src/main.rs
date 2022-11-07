use std::fs::File;
use std::io::Read;
use std::env;
fn main() {
    let args: Vec<String> = env::args().collect();
    if env::args().len() < 2 {
        panic!("number of args low");
    }
    let mut file = File::open(args[1].trim()).expect("expected file to open");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("expected read_to_string to work");
    print!("{}", contents);
}
