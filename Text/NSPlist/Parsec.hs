module Text.NSPlist.Parsec (
    pNSPlist
  ) where


import Data.Word (Word8)
import Numeric (readHex)
import Control.Applicative ((<$>), (<*), (*>), (<*>), pure, (<|>), empty)
import Text.Parsec (Parsec)
import Text.Parsec.Char (char, spaces, alphaNum, string, anyChar, oneOf, digit)
import Text.Parsec.Prim (getInput, setInput, try, many)
import Text.Parsec.Combinator (many1, sepEndBy, endBy, between, option)

import Text.NSPlist.Types (NSPlistValue(..))


pNSPlist :: Parsec String u NSPlistValue
pNSPlist = pNonContent *> pNSPlistValue

pNSPlistValue :: Parsec String u NSPlistValue
pNSPlistValue = pArray <|> pDictionary <|> pBinary <|> (NSString <$> pString)

pCharNonContent :: Char -> Parsec String u Char
pCharNonContent c = char c <* pNonContent

-- | Parser that parses non-content elements, that is, comments and spaces
pNonContent :: Parsec String u ()
pNonContent = spaces <* (pComment *> pNonContent <|> pure ())

pComment :: Parsec String u String
pComment = (try pBlockComment <|> try pLineComment) <* spaces

pBlockComment :: Parsec String u String
pBlockComment = const <$> string "/*" <*> recBlockComment <* spaces
 where
  recBlockComment = try (const "" <$> string "*/")
                    <|> ((:) <$> anyChar <*> recBlockComment)

pLineComment :: Parsec String u String
pLineComment = const <$> string "//" <*> recLineComment <* spaces
 where
  recLineComment = try (const "" <$> char '\n')
                   <|> ((:) <$> anyChar <*> recLineComment)

-- apparently the elements of an array may be separated by commas, but an array
-- may as well have a comma after the final element
pArray :: Parsec String u NSPlistValue
pArray =
  between (pCharNonContent '(')
          (pCharNonContent ')')
          (NSArray <$> sepEndBy pNSPlistValue (pCharNonContent ','))

pDictionary :: Parsec String u NSPlistValue
pDictionary =
  between (pCharNonContent '{')
          (pCharNonContent '}')
          (NSDictionary <$> endBy pKeyValue (pCharNonContent ';'))

pKeyValue :: Parsec String u (String, NSPlistValue)
pKeyValue = (,) <$> pString <* pCharNonContent '=' <*> pNSPlistValue

pString :: Parsec String u String
pString = (pQuotedString <|> pNonQuotedString) <* pNonContent

-- | Parser for non-quoted string, it looks like names without quotes
-- may contain '.', '_', and '/' besindes alpha numberic characters
pNonQuotedString :: Parsec String u String
pNonQuotedString = many1 (oneOf "._/" <|> alphaNum)

pQuotedString :: Parsec String u String
pQuotedString = do
  input <- getInput
  case reads input of
       ((str, rest):_) -> const str <$> setInput rest
       _               -> empty

-- | Parses data that is represented by hexadecimal codes
pBinary :: Parsec String u NSPlistValue
pBinary =
  between (pCharNonContent '<') (pCharNonContent '>') (NSData <$> many pHexWord)

pHexWord :: Parsec String u Word8
pHexWord = (\cs -> readHex' . (cs++)) <$> pHexChar <*> option [] pHexChar
 where
  readHex' str = fst (head (readHex str))

pHexChar :: Parsec String u String
pHexChar = (:[]) <$> (digit <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']) <* spaces
