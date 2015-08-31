{-# LANGUAGE FlexibleInstances #-}
import Data.Boombox.Drive as B
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Data.Functor.Identity
import Control.Applicative
import Data.Monoid

instance Functor m => Parsing (PlayerT ParseError s m) where
  try = B.try
  m <?> name = PlayerT $ \s ce cs -> unPlayerT m s (\s' -> ce s' . Expecting [name]) cs
  unexpected s = failed $ Unexpected s
  notFollowedBy m = B.try $ PlayerT $ \s ce cs -> unPlayerT m s (\s' _ -> cs s' ()) (\s' -> ce s' . Unexpected . show)
  eof = PlayerT $ \s ce cs -> if null s
    then Partial (cs [] ()) (\s' -> ce [s'] (Unexpected "leftover"))
    else ce s (Unexpected "leftover")

instance Functor m => CharParsing (PlayerT ParseError Char m) where
  satisfy p = do
    x <- await <|> failed EndOfStream
    if p x then return x else leftover [x] >> failed (Unexpected (show x))

instance Functor m => TokenParsing (PlayerT ParseError Char m)

data ParseError = EndOfStream
    | Unexpected String
    | Expecting [String] ParseError
    | Unknwown

instance Monoid ParseError where
  mempty = Unknwown
  mappend (Expecting s p) (Expecting t q) = Expecting (s ++ t) p
  mappend _ a = a

toEnglish :: ParseError -> String
toEnglish EndOfStream = "Unexpected end of stream"
toEnglish (Unexpected s) = "Unexpected " ++ s
toEnglish (Expecting s EndOfStream) = "expecting " ++ unwords s ++ ", but got end of stream"
toEnglish (Expecting s (Unexpected e)) = "expecting " ++ unwords s ++ ", but got " ++ e
toEnglish (Expecting s (Expecting t e)) = "expecting " ++ unwords s ++ ", but failed due to: " ++ toEnglish e
toEnglish (Expecting s e) = "expecting " ++ unwords s ++ ", but failed due to: " ++ toEnglish e
toEnglish Unknwown = "an unknwown error"

testParser :: (Show a) => Parser a -> String -> IO ()
testParser m s = case runIdentity $ finishDrive $ supplyDrive s $ runPlayerT m of
  (Right a, lo) -> print a >> print lo
  (Left e, lo) -> putStrLn (toEnglish e) >> print lo

type Parser = PlayerT ParseError Char Identity

expr :: Parser Int
expr   = term   `chainl1` addop

term :: Parser Int
term   = factor `chainl1` mulop

factor :: Parser Int
factor = parens expr <|> fmap fromEnum integer

mulop :: Parser (Int -> Int -> Int)
mulop  = (*) <$ symbol "*"
    <|> div <$ symbol "/"

addop :: Parser (Int -> Int -> Int)
addop  = (+) <$ symbol "+"
    <|> (-) <$ symbol "-"
