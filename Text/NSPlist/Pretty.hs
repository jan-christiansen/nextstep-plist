module Text.NSPlist.Pretty (
    prettyNSPlist
  ) where


import Text.PrettyPrint (Doc, comma, semi, equals, parens, braces, fsep,
                         punctuate, text, char, (<+>), (<>))
import Numeric (showHex)

import Text.NSPlist.Types (NSPlistValue(..))


angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

prettyNSPlist :: NSPlistValue -> Doc
prettyNSPlist (NSString str) = prettyString str
prettyNSPlist (NSData ws) = angles (fsep (map prettyHex ws))
 where
  prettyHex w = text (reverse (take 2 (reverse ('0':showHex w ""))))
prettyNSPlist (NSArray values) =
  parens (fsep (punctuate comma (map prettyNSPlist values)))
prettyNSPlist (NSDictionary values) = braces (fsep (map prettyKeyValue values))

prettyKeyValue :: (String, NSPlistValue) -> Doc
prettyKeyValue (key, value) =
  prettyString key <+> equals <+> prettyNSPlist value <> semi

prettyString :: String -> Doc
prettyString str = text (show str)
