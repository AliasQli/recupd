{-# LANGUAGE TupleSections #-}

module Internal.Parser (Field, splitFields) where

import Data.Char
import Data.Bifunctor
 
newtype Parser a = Parser {runParser :: String -> Either String (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser (Right . (a,))
  Parser f <*> Parser p = Parser $ \s -> do
    (f', s') <- f s 
    (p', s'') <- p s'
    pure (f' p', s'')

type Field = (String, Bool {- isUpdate -}, String)

splitFields :: String -> Either String [Field]
splitFields = go . dropWhile isSpace
  where
    go "" = pure []
    go expr = do
      (m, r) <- runParser fieldParser expr
      (m :) <$> splitFields r

skipSpaces :: Parser ()
skipSpaces = Parser (Right . ((),) . dropWhile isSpace)

fieldParser :: Parser Field
fieldParser = (,,) <$> identifierParser <*> assignParser <*> exprParser

identifierParser :: Parser String
identifierParser = Parser go <* skipSpaces
  where
    go (c:s) | isAlpha c = pure (withMatch c $ go' s)
    go _                 = Left "Identifier should start with a letter"
    go' s = (takeWhile cond s, dropWhile cond s)
      where cond c = isAlphaNum c || c `elem` ("_'" :: [Char])

assignParser :: Parser Bool
assignParser = Parser go <* skipSpaces
  where
    go ('$':'=':s) = pure (True, s)
    go ('=':s)     = pure (False, s)
    go _           = Left "Not one of '$=' or '='"

type ParseString = String -> (String, String)

-- From dump-0.8.2.Internal.Parser, author Milán Nagy, originally licensed as MIT
exprParser :: Parser String
exprParser = Parser (Right . parseExpr) <* skipSpaces
  where
    parseExpr :: ParseString
    parseExpr = parseExp' []

    parseExp' :: [Char] -> ParseString
    parseExp' _ "" = ("", "")
    parseExp' [] (',':xs) = ("", xs)
    parseExp' (s:tack) (x:xs) | x == s = withMatch x $ parseExp' tack xs
    parseExp' stack (x:xs) = withMatch x $ case x of
      '('  -> parseExp' (')':stack) xs
      '['  -> parseExp' (']':stack) xs
      '"'  -> parseLeaf (parseExp' stack) '"'  xs
      '\'' -> parseLeaf (parseExp' stack) '\'' xs
      _    -> parseExp' stack xs

    parseLeaf :: ParseString -> Char -> ParseString
    parseLeaf _ _ "\\" = ("\\", "")
    parseLeaf _ _ ""   = ("",   "")
    parseLeaf cont cc (x:xs) = withMatch x result
      where
      result
        | x == cc   = cont xs
        | x == '\\' = withMatch y $ parseLeaf cont cc ys
        | otherwise = parseLeaf cont cc xs
      (y:ys) = xs

withMatch :: a -> ([a], [b]) -> ([a], [b])
withMatch x (m, r) = (x:m, r)
