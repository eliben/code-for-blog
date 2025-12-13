use mdbook_preprocessor::book::Book;
use mdbook_preprocessor::errors::Result;
use mdbook_preprocessor::{Preprocessor, PreprocessorContext};
use std::io;

struct Narcissist;

impl Preprocessor for Narcissist {
    fn name(&self) -> &str {
        "narcissist"
    }

    fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book> {
        book.for_each_chapter_mut(|ch| {
            // Use regexp to replace all instance of standalone "I" with
            // bolded "I (name)", where "name" is the name of the first author
            // in the book's metadata.
            if let Some(name) = ctx.config.book.authors.get(0) {
                ch.content = regex::Regex::new(r"\bI\b")
                    .unwrap()
                    .replace_all(&ch.content, format!("**I ({name})**"))
                    .to_string();
            }

            ch.content.push_str("\n\nRewrite all your code in Rust!!\n");
        });
        Ok(book)
    }
}

pub fn handle_preprocessing() -> Result<()> {
    let pre = Narcissist;
    let (ctx, book) = mdbook_preprocessor::parse_input(io::stdin())?;

    let processed_book = pre.run(&ctx, book)?;
    serde_json::to_writer(io::stdout(), &processed_book)?;

    Ok(())
}

fn main() {
    let mut args = std::env::args().skip(1);
    match args.next().as_deref() {
        Some("supports") => {
            // Supports all renderers.
            return;
        }
        Some(arg) => {
            eprintln!("unknown argument: {arg}");
            std::process::exit(1);
        }
        None => {}
    }

    if let Err(e) = handle_preprocessing() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
