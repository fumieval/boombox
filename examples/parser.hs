{-# LANGUAGE FlexibleInstances #-}
import Data.Boombox.Drive
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Data.Functor.Identity
import Control.Applicative

instance Functor m => Parsing (PlayerT ParseError s m) where
  try = trackPlayerT
  m <?> name = PlayerT $ \s ce cs -> unPlayerT m s (ce . Expecting name) cs
  unexpected s = failed $ Unexpected s
  notFollowedBy m = try $ PlayerT $ \s ce cs -> unPlayerT m s (\_ -> cs s ()) (const $ ce . Unexpected . show)
  eof = PlayerT $ \s ce cs -> if null s
    then Partial (cs [] ()) (const $ ce (Unexpected "leftover"))
    else ce (Unexpected "leftover") 

instance Functor m => CharParsing (PlayerT ParseError Char m) where
  satisfy p = do
    x <- awaitError EndOfStream
    if p x then return x else failed $ Unexpected (show x)

instance Functor m => TokenParsing (PlayerT ParseError Char m)

data ParseError = EndOfStream
    | Unexpected String
    | Expecting String ParseError
    | Unknwown

instance Monoid ParseError where
  mempty = Unknwown
  mappend _ a = a

toEnglish :: ParseError -> String
toEnglish EndOfStream = "Unexpected end of stream"
toEnglish (Unexpected s) = "Unexpected " ++ s
toEnglish (Expecting s EndOfStream) = "expecting " ++ s ++ ", but got end of stream"
toEnglish (Expecting s (Unexpected e)) = "expecting " ++ s ++ ", but got " ++ e
toEnglish (Expecting s (Expecting t e)) = "expecting " ++ s ++ " (i.e. " ++ t ++ "), but failed due to: " ++ toEnglish e
toEnglish (Expecting s e) = "expecting " ++ s ++ ", but failed due to: " ++ toEnglish e
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
