import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- These two functions return the *length* of the longest subsequence
-- (it wouldn't be too difficult to return the actual sequence)

longestIncreasingSubsequence :: [Integer] -> Int
longestIncreasingSubsequence = f 0 Map.empty
  where f maxLen _ [] = maxLen
        -- `ends` is the inverse of the function
        -- k \in [0, maxLen] -> smallest last element of an incr. seq.
        --                      of length k within the prefix already seen
        f maxLen ends (x:xs) = case Map.lookupGE x ends of
          Nothing     -> f (maxLen + 1) (Map.insert x (maxLen + 1) ends) xs
          Just (y, k) -> f maxLen       (updateBest ends)                xs
          -- updateBest is suboptimal, x has the same position as y in the tree
            where updateBest = Map.insert x k . Map.delete y

longestNonDecreasingSubsequence :: [Integer] -> Int
longestNonDecreasingSubsequence = f 0 Set.empty
  where f maxLen _ [] = maxLen
        -- replace lookupGE with lookupGT for max non-decreasing seq
        -- now our function is not injective ->
        -- multivalued inverse, represented by its graph
        f maxLen ends (x:xs) = case Set.lookupGT (x, Right ()) ends of
          Nothing -> f (maxLen + 1) (Set.insert (x, Left (maxLen + 1)) ends) xs
          Just (y, Left k) -> f maxLen (updateBest ends) xs
            where updateBest = Set.insert (x, Left k) . Set.delete (y, Left k)
 
-- Application: http://prologin.org/training/challenge/demi2010/urgence_reseau
main :: IO ()
main = do
  n <- read <$> getLine
  ls <- sequenceA $ replicate n getLine
  let fibers = [let [xg, xd] = map read . words $ l in (xg, xd) | l <- ls]
      sorted = map snd . sort $ fibers
  print $ longestNonDecreasingSubsequence sorted

