module Errors (
      printSuperPolicy
    , printErrors
    ) where

        
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Env
import AST


printErrors :: Errors -> String
printErrors err = do 
    case err of
        NoGroup -> do "There is no group defined."
        NoRelation -> do "There is no relation."  
        NoBindingForVariable a -> do "No available binding for variable " ++ T.unpack(a) ++ "."    
        NoBindingForPolicy a -> do "No available binding for policy " ++ T.unpack(a) ++ "."    
        UnsupportedOperation s -> do "Unsuported operation: " ++ s
        NoLanguageOption at al -> do "The aspect tag "++T.unpack(at) ++ " has no langauge option " ++ T.unpack(tdnsToString al)
        DefaultError s -> do "An error has occurred: " ++ s
        BadPredicate s -> do "Invalid predicate: " ++ s
        BadComparison -> do "Invalid comparison operator."



-------------------------------------
--- Pretty printing superpolicies ---
-------------------------------------

printSuperPolicy :: SuperPolicy -> String
printSuperPolicy (SuperPolicy pols) = do "SuperPolicy:" ++  policiesToString pols

policiesToString :: [Policy] -> String
policiesToString (p1:[]) = do policyToString p1
policiesToString (p1:pols) = do policyToString p1 ++ ", " ++ policiesToString pols



policyToString :: Policy -> String
policyToString PBot = "_"
policyToString PTop = "*"
policyToString (Policy aspects) = do "{" ++ aspectsToString (M.toList aspects) ++  "}"



aspectsToString :: [(ATag, ALang)] -> String
aspectsToString ((tag, aspect):[]) = do (T.unpack tag) ++ ": " ++ T.unpack (tdnsToString aspect)
aspectsToString ((tag, aspect):asps) = do (T.unpack tag) ++ ": " ++ T.unpack (tdnsToString aspect) ++ ", " ++ aspectsToString asps


tdnsToString :: ALang -> T.Text
tdnsToString LTop = T.pack "*"
tdnsToString LBot = T.pack "_"
tdnsToString (TDNS ( Node (Atom f) (Leaf LTop)      (Leaf LTop)))  = f
tdnsToString (TDNS ( Node (Atom f) (Leaf LTop)      (Leaf (Atom t))))   = T.pack $ getString f T.empty t
tdnsToString (TDNS ( Node (Atom f) (Leaf (Atom s))  (Leaf LTop))) = T.pack $ getString f s T.empty
tdnsToString (TDNS ( Node (Atom f) (Leaf (Atom s))  (Leaf (Atom t))))   = T.pack $ getString f s t

getString :: T.Text -> T.Text -> T.Text -> String
getString one two three
    | two == T.empty && three == T.empty = T.unpack one
    | two == T.empty = (T.unpack one ) ++ "." ++ (T.unpack three )
    | three == T.empty =  (T.unpack one ) ++ "[" ++ (T.unpack two ) ++ "]"
    | otherwise = (T.unpack one ) ++ "[" ++ (T.unpack two ) ++ "]." ++ (T.unpack three )