module Common.Reduce
    ( reduceSuperPolicy
    ) where

import           Common.AST
import qualified Data.Map.Strict as M
import qualified Data.PartialOrd as P

----------------------------
-- | Reducing super-policies
----------------------------

reduceSuperPolicy :: SuperPolicy -> SuperPolicy
reduceSuperPolicy = SuperPolicy . P.maxima
                                . map reducePolicy
                                . policies

reducePolicy :: Policy -> Policy
reducePolicy (Policy aspects) = Policy $ M.map reduceALang aspects
reducePolicy p = p

reduceALang :: ALang -> ALang
reduceALang (TDNS t) = TDNS (reduceTree t)
reduceALang a = a

reduceTree :: Tree ALang -> Tree ALang
reduceTree (Leaf d) = Leaf $ reduceALang d
reduceTree (Node d (Node LTop (Leaf LTop) (Leaf LTop)) r1) =
    reduceTree $ Node d (Leaf LTop) r1
reduceTree (Node d b (Node LTop (Leaf LTop) (Leaf LTop))) =
    reduceTree $ Node d b (Leaf LTop)
reduceTree (Node LBot _ _) = Leaf LBot
reduceTree t@(Node alang b r) =
    let reduced = Node (reduceALang alang)
                       (reduceTree b)
                       (reduceTree r)
    in if reduced /= t
       then reduceTree reduced
       else t