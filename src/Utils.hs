module Utils where

import Control.Concurrent.Async
import Data.Functor

import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq

race3_ :: IO a -> IO a -> IO a -> IO ()
race3_ io1 io2 io3 = withAsync io1 $ \a1 ->
  withAsync io2 $ \ a2 -> 
    withAsync io3 $ \ a3 ->  
      void $ waitAnyCancel [a1, a2, a3]

race4_ :: IO a -> IO a -> IO a -> IO a -> IO ()
race4_ io1 io2 io3 io4 = withAsync io1 $ \a1 ->
  withAsync io2 $ \ a2 -> 
    withAsync io3 $ \ a3 ->  
      withAsync io4 $ \ a4 ->  
        void $ waitAnyCancel [a1, a2, a3, a4]

first :: (a, b, c) -> a
first (a,_,_) = a

second :: (a, b, c) -> b
second (_,b,_) = b

third :: (a, b, c) -> c
third (_,_,c) = c

seqLast :: Seq a -> Maybe a
seqLast s
  | not (Seq.null s) = Just $ Seq.index s (Seq.length s - 1)
  | otherwise        = Nothing
