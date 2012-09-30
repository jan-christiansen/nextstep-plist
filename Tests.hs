module Tests where


import Test.QuickCheck (quickCheck)
import Text.Printf (printf)

import Text.NSPlist (NSPlistValue, parseNSPlist, showNSPlist)


-- | QuickCheck property that checks whether parsing a printed value
-- is the identity
prop_NSIdentity :: NSPlistValue -> Bool
prop_NSIdentity value =
  case parseNSPlist (showNSPlist value) of
       Left _    -> False
       Right res -> res == value

tests :: [(String, IO ())]
tests = [("NextStep parser/show identity", quickCheck prop_NSIdentity)]

main :: IO ()
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
