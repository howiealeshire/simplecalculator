module Main where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import System.Environment
import Debug.Trace


type Parser = Parsec Void String

-- to fix error, maybe add folding code? Basically, currently, it's parsing everything but the last arg. Maybe has something to do with "head", not sure.

main = do
  input <- fmap head getArgs
  readExpr input
  
  
--parseTest aExpr input

readExpr :: String -> IO ()
readExpr input = do
  case (parse aExpr "" input) of
    Left err -> putStr (parseErrorPretty err)
    Right xs -> print $ unpackNum xs

  
singleLetterP :: Parser Char
singleLetterP = char 'h'


-- define our calculator data types

data AExpr
  = IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)


data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  deriving (Show)


-- add support for parens in a bit
showAExpr :: AExpr -> String
showAExpr (IntConst num) = show num
showAExpr (Neg contents) = "-" ++ showAExpr contents
showAExpr (ABinary Add ex1 ex2) = (showAExpr ex1) ++ "+" ++ (showAExpr ex2)
showAExpr (ABinary Subtract ex1 ex2) = (showAExpr ex1) ++ "-" ++ (showAExpr ex2)
showAExpr (ABinary Multiply ex1 ex2) = (showAExpr ex1) ++ "*" ++ (showAExpr ex2)
showAExpr (ABinary Divide ex1 ex2) = (showAExpr ex1) ++ "/" ++ (showAExpr ex2)
showAExpr (ABinary Mod ex1 ex2) = (showAExpr ex1) ++ "%" ++ (showAExpr ex2)

{-
eval :: AExpr -> AExpr
eval (IntConst num) = IntConst num
eval (Neg contents) = Neg (IntConst (unpackNum contents))
eval (ABinary Add ex1 ex2) = ABinary Add (IntConst $ unpackNum ex1) (IntConst $ unpackNum ex2)
eval (ABinary Subtract ex1 ex2) = ABinary Subtract (IntConst $ unpackNum ex1) (IntConst $ unpackNum ex2)
eval (ABinary Multiply ex1 ex2) = ABinary Multiply (IntConst $ unpackNum ex1) (IntConst $ unpackNum ex2)
eval (ABinary Divide ex1 ex2) = ABinary Divide (IntConst $ unpackNum ex1) (IntConst $ unpackNum ex2)
eval (ABinary Mod ex1 ex2) = ABinary Mod (IntConst $ unpackNum ex1) (IntConst $ unpackNum ex2)
-}

unpackNum :: AExpr -> Integer
unpackNum (IntConst num) = num
unpackNum (Neg contents) = - (unpackNum contents)
unpackNum (ABinary Add ex1 ex2) = (unpackNum ex1) + (unpackNum ex2)
unpackNum (ABinary Subtract ex1 ex2) = (unpackNum ex1) - (unpackNum ex2)
unpackNum (ABinary Multiply ex1 ex2) = (unpackNum ex1) * (unpackNum ex2)
unpackNum (ABinary Divide ex1 ex2) = (unpackNum ex1) `div` (unpackNum ex2)
unpackNum (ABinary Mod ex1 ex2) = (unpackNum ex1) `mod` (unpackNum ex2)







-- Lexer section

-- "Space consumer". The last 2 args are empty bc there are no comments
-- in the language
sc :: Parser ()
sc = L.space space1 empty empty



lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal


aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators
 

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/")
    , InfixL (ABinary Mod      <$ symbol "%")]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]


aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> IntConst <$> integer








