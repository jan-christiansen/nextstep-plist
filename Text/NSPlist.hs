-- | Parsing and Printing NextStep style (old style) plist files

module Text.NSPlist (
    NSPlistValue(..),

    parseNSPlist,
    readNSPlistFromFile,

    showNSPlist,
    writeNSPlistToFile
  ) where


import Text.Parsec (runParser)
import Text.Parsec.Error (ParseError)
import Text.PrettyPrint (render)

import Text.NSPlist.Types (NSPlistValue(..))
import Text.NSPlist.Parsec (pNSPlist)
import Text.NSPlist.Pretty (prettyNSPlist)


parseNSPlist :: String -> Either ParseError NSPlistValue
parseNSPlist = runParser pNSPlist () ""

readNSPlistFromFile :: FilePath -> IO NSPlistValue
readNSPlistFromFile filePath = do
  file <- readFile filePath
  case runParser pNSPlist () filePath file of
       Left parseError -> error ("Cannot parse file " ++ filePath
                                 ++ ", error was " ++ show parseError)
       Right obj       -> return obj

writeNSPlistToFile :: FilePath -> NSPlistValue -> IO ()
writeNSPlistToFile filePath = writeFile filePath . showNSPlist

showNSPlist :: NSPlistValue -> String
showNSPlist = render . prettyNSPlist
