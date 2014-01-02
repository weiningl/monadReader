-- Abitrary nested parentheses parsing
import Control.Applicative
import Parser

data Nesting = One Char
             | Many [Nesting] deriving (Show, Eq)

char :: Parser Char Nesting
char = fmap One $ check (not . flip elem "()") item

level :: Parser Char Nesting
level = literal '(' *> (fmap Many $ many element) <* literal ')'

element :: Parser Char Nesting
element = char <|> level

parseNest :: Parser Char [Nesting]
parseNest = many element
