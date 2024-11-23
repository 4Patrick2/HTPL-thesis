{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Common.Pretty
    ( module Common.Pretty
    ) where

import           Data.List ( intercalate )
import qualified Data.Text as T

----------------------------
-- | Custom Pretty typeclass
----------------------------

class Pretty a where
    pretty :: a -> String


----------------------
-- | Utility functions
----------------------

(<+>) :: String -> String -> String
str1 <+> str2 = str1 <> " " <> str2

(<\>) :: String -> String -> String
str1 <\> str2 = str1 <> "\n" <> str2

(<\\>) :: String -> String -> String
str1 <\\> str2 = str1 <> "\n\n" <> str2

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty

prettySepBy :: Pretty a => String -> [a] -> String
prettySepBy sep = intercalate sep . map pretty

prettySepBy' :: Pretty a => String -> [a] -> String
prettySepBy' sep = prettySepBy (sep <> " ")

prettyPadSepBy :: Pretty a => String -> [a] -> String
prettyPadSepBy sep = prettySepBy (space <> sep <> space)

prettySepEndBy :: Pretty a => String -> [a] -> String
prettySepEndBy sep = concatMap ((<> sep) . pretty)

parens, brackets, braces :: String -> String
parens str   = "(" <> str <> ")"
brackets str = "[" <> str <> "]"
braces str   = "{" <> str <> "}"

semi, colon, comma, dot, equal :: String
semi  = ";"; colon = ":"
comma = ","; dot   = "."
equal = "="

line, space :: String
line = "\n"; space = " "

empty :: String
empty = mempty



---------------------
-- | Utility function
---------------------

convertResult :: Pretty a => Either a b -> Either String b
convertResult e = case e of
    Left err -> Left $ pretty err
    Right res -> Right res


--------------
-- | Instances
--------------

instance Pretty String where
    pretty :: String -> String
    pretty str = str

instance Pretty T.Text where
    pretty :: T.Text -> String
    pretty = T.unpack

instance Pretty Int where
    pretty :: Int -> String
    pretty = show

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
    pretty :: [a] -> String
    pretty = brackets . intercalate ", " . map pretty