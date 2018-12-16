module Queue 
  ( push
  , pushList
  , pop
  , null
  , empty
  ) where

import           Prelude hiding (null)
import qualified Data.Sequence as S


newtype Queue a = Queue (S.Seq a) deriving Show

push v (Queue q) = Queue (q S.:|> v)

pushList (x:xs) (Queue q) = push x (pushList xs (Queue q))
pushList [] q = q

pop (Queue (v S.:<| q)) = (Just v, Queue q)
pop (Queue S.Empty) = (Nothing, Queue S.Empty)

null (Queue q) = S.null q

empty = Queue S.empty
