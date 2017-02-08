{-# Language GADTs                     #-}
{-# Language ExistentialQuantification #-}
{-# Language KindSignatures            #-}
{-# Language TemplateHaskell           #-}

import ConstructorInfo
import Text.Show.Pretty (ppShow)
import Language.Haskell.TH

data T :: (* -> *) -> * -> * where
  C1 :: Int -> T f a
  C2 :: { c2field :: f Int } -> TUnit (I f)
  C3,C4 :: [a] -> T f (Maybe a)

type TUnit f = T f ()
type I (f :: * -> *) = f

data U = forall a. Show a => MkU a

return []

main =
  do putStrLn $(stringE . ppShow =<< normalizeInfo =<< reify ''T)
     putStrLn $(stringE . ppShow =<< normalizeInfo =<< reify ''U)
