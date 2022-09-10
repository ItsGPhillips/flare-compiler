use token::Token;

pub mod text_input_tokenstream;
pub mod token;
pub mod tokenstream;

pub use tokenstream::TokenStream;

/// Types that provide the base tokens for a token stream should impliment this trait.
pub trait TokenStreamSource: Iterator<Item = Token> + Clone {}

#[cfg(test)]
mod tests {
    // TODO: write tests
}
