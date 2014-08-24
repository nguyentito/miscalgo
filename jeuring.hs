-- Extract from Johan Jeuring's 'palindromes' package

-- | palindromesAroundCentres is the central function of the module. It returns
--   the list of lenghths of the longest palindrome around each position in a
--   string.

import qualified Data.ByteString as B

palindromesAroundCentres        :: B.ByteString -> [Int]
palindromesAroundCentres input  =  reverse $ extendPalindrome input 0 0 []

extendPalindrome :: B.ByteString -> Int -> Int -> [Int] -> [Int]
extendPalindrome input rightmost currentPalindrome currentMaximalPalindromes 
  | rightmost > last
      -- reached the end of the array
      =  finalPalindromes currentPalindrome currentMaximalPalindromes (currentPalindrome:currentMaximalPalindromes)
  | rightmost-currentPalindrome == first ||
    not (B.index input rightmost == B.index input (rightmost-currentPalindrome-1))
      -- the current palindrome extends to the start of the array, 
      -- or it cannot be extended 
      =  moveCenter input rightmost (currentPalindrome:currentMaximalPalindromes) currentMaximalPalindromes currentPalindrome 
  | otherwise                                           
      -- the current palindrome can be extended
      =  extendPalindrome input (rightmost+1) (currentPalindrome+2) currentMaximalPalindromes      
  where  first = 0
         last  = B.length input - 1

moveCenter :: B.ByteString -> Int -> [Int] -> [Int] -> Int -> [Int]
moveCenter input rightmost currentMaximalPalindromes previousMaximalPalindromes nrOfCenters
  | nrOfCenters == 0
      -- the last centre is on the last element: try to extend the tail of length 1
      =  extendPalindrome input (rightmost+1) 1 currentMaximalPalindromes
  | nrOfCenters-1 == head previousMaximalPalindromes
      -- the previous element in the centre list reaches exactly to the end of the last 
      -- tail palindrome use the mirror property of palindromes to find the longest tail palindrome
      =  extendPalindrome input rightmost (head previousMaximalPalindromes) currentMaximalPalindromes
  | otherwise
      -- move the centres one step add the length of the longest palindrome to the centres
      =  moveCenter input rightmost (min (head previousMaximalPalindromes) (nrOfCenters-1):currentMaximalPalindromes) (tail previousMaximalPalindromes) (nrOfCenters-1)

finalPalindromes :: Int -> [Int] -> [Int] -> [Int]
finalPalindromes nrOfCenters previousMaximalPalindromes currentMaximalPalindromes  
  | nrOfCenters == 0
      =  currentMaximalPalindromes
  | nrOfCenters > 0
      =  finalPalindromes (nrOfCenters-1) (tail previousMaximalPalindromes) (min (head previousMaximalPalindromes) (nrOfCenters-1):currentMaximalPalindromes)
  | otherwise  
      =  error "finalCentres: input < 0"               
