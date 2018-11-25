module DataType
(   
    Exp(..)
   ,OptList(..)
)where
data Exp = Opt [Char] | Exp [Exp] | Num Double | Begin | End
    deriving Show
data OptList = OptList Int [(Char,(Double->Double->Double))]
