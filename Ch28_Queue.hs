module Ch28_Queue where

import qualified Data.Sequence as S



-- | A generic FIFO Queue
class Queue q where
  empty    :: q a                    -- ^ the empty queue
  push     :: a -> q a -> q a        -- ^ add item to queue
  pop      :: q a -> Maybe (a, q a)  -- ^ remove next item
  fromList :: [a] -> q a             -- ^ queue from a list, front is first out
  lengthQ  :: q a -> Int             -- ^ number of elements in the queue
  isEmpty  :: q a -> Bool            -- ^ does the queue have elements?
  isEmpty  = (0 ==) . lengthQ



-- | Efficient queue from Okasaki
--
-- Whenever dequeue list is empty, reverse enqueue list and assign it
-- to the dequeue list
data EQueue a =
  EQueue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

emptyEQ :: EQueue a
emptyEQ = EQueue [] []

pushEQ :: a -> EQueue a -> EQueue a
pushEQ a (EQueue e []) = EQueue [] $ a : reverse e
pushEQ a (EQueue e d) = EQueue (a:e) d

popEQ :: EQueue a -> Maybe (a, EQueue a)
popEQ (EQueue [] []) = Nothing
popEQ (EQueue e []) =
  let d = reverse e
  in Just (head d, EQueue [] $ tail d)
popEQ (EQueue e d) = Just (head d, EQueue e $ tail d)

instance Queue EQueue where
  empty = emptyEQ
  push = pushEQ
  pop = popEQ
  fromList xs = EQueue [] xs
  lengthQ (EQueue e d) = length e + length d



-- | List-based Queue
--
-- Backed by a single list. Naive design for comparison purposes. Not
-- for actual use.
newtype ListQueue a =
  ListQueue [a]
  deriving (Eq, Show)

instance Queue ListQueue where
  empty = ListQueue $ []
  push a (ListQueue xs) = ListQueue $ xs ++ [a]
  pop (ListQueue []) = Nothing
  pop (ListQueue xs) = Just (head xs, ListQueue $ tail xs)
  fromList xs = ListQueue xs
  lengthQ (ListQueue xs) = length xs



-- | Sequence-based Queue
newtype SeqQueue a =
  SeqQueue (S.Seq a)

instance Queue SeqQueue where
  empty = SeqQueue $ S.empty
  push a (SeqQueue s) = SeqQueue (s S.|> a)
  pop (SeqQueue s) =
    case S.viewl s of
      S.EmptyL -> Nothing
      (a S.:< rest) -> Just (a, SeqQueue rest)
  fromList xs = SeqQueue $ S.fromList xs
  lengthQ (SeqQueue s) = S.length s
