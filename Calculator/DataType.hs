module DataType
(   
    Exp(..)
   ,OptList(..)
   ,OptList_All
)where
data Exp = Opt [Char] | Exp [Exp] | Num Double | Begin | End
    deriving (Show,Eq)
data OptList = OptList Int [([Char],(Double->Double->Double))]
type OptList_All=[OptList]