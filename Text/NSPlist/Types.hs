module Text.NSPlist.Types where


import Control.Monad (liftM)
import Data.Word (Word8)
import Test.QuickCheck (Arbitrary(..), sized, resize, oneof)


-- | Data type that represents NextStep style (old style) Plist values
data NSPlistValue = NSString String
                  | NSDictionary [(String, NSPlistValue)]
                  | NSArray [NSPlistValue]
                  | NSData [Word8]
  deriving (Eq, Show)


instance Arbitrary NSPlistValue where
  arbitrary = sized (\s -> oneof [liftM NSString (resizedArb s),
                                  liftM NSData (resizedArb s),
                                  liftM NSArray (resizedArb s),
                                  liftM NSDictionary (resizedArb s)])
   where
    resizedArb s = resize (s `div` 4) arbitrary

  shrink (NSString str)         = liftM NSString (shrink str)
  shrink (NSData d)             = liftM NSData (shrink d)
  shrink (NSArray values)       = liftM NSArray (shrink values)
  shrink (NSDictionary values)  = liftM NSDictionary (shrink values)
