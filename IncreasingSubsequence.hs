import Control.Applicative
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map

-- returns *length* of LIS
-- (it wouldn't be too difficult to return the actual sequence)
longestIncreasingSubsequence :: [Integer] -> Int
longestIncreasingSubsequence = f 0 Map.empty
  where f maxLen _ [] = maxLen
        -- `ends` is the inverse of the function
        -- k \in [0, maxLen] -> smallest last element of an incr. seq.
        --                      of length k within the prefix already seen
        f maxLen ends (x:xs) = case Map.lookupGE x ends of
          Nothing -> f (maxLen + 1) (Map.insert x (maxLen + 1) ends) xs
          Just (y, k) | x < y     -> f maxLen (updateBest ends) xs
                      | otherwise -> f maxLen ends              xs
          -- updateBest is suboptimal, x has the same position as y in the tree
            where updateBest = Map.insert x k . Map.delete y

-- Application: http://prologin.org/training/challenge/demi2010/urgence_reseau
main :: IO ()
main = do
  n <- read <$> getLine
  lines <- sequenceA $ replicate n getLine
  let fibers = [let [xg, xd] = map read . words $ l in (xg, xd) | l <- lines]
      sorted = map snd . sortBy (compare `on` fst) $ fibers
      -- trick to convert nondecreasing sequences to increasing sequences
      altered = zipWith (+) [0..] sorted
  print $ longestIncreasingSubsequence altered

