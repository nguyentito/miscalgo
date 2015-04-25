--------------------------------------------------------
-- Arithmetic calculator functions                    --
-- They all parse and evaluate different notations    --
--------------------------------------------------------

import Data.List
import Data.Char
import Control.Applicative
import Control.Monad


-- Common declarations for calculators --
-----------------------------------------

type Calculator = String -> Maybe Integer

opAList = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]
tryGetOp = flip lookup opAList
tryParseNum = foldM f 0
    where f acc c | isDigit c = Just $ acc*10 + fromIntegral (digitToInt c)
                  | otherwise = Nothing


-- Reverse Polish Notation calculator --
----------------------------------------

calcRPN :: Calculator
calcRPN str = do
  [finalResult] <- foldM updateStack [] $ words str
  return finalResult
    where updateStack currentStack newStrToken = tryUpdateWithOp currentStack newStrToken
                                                 <|> (:currentStack) <$> (tryParseNum newStrToken)
          tryUpdateWithOp (x1:x2:xs) str = do
                op <- tryGetOp str
                return $ (op x2 x1):xs -- it's X2 first, then X1
          tryUpdateWithOp _ _ = Nothing


-- Polish Notation calculator --
--------------------------------

data PNTree = PNNumNode Integer
            | PNOpNode (Integer -> Integer -> Integer) PNTree PNTree

calcPN :: Calculator
calcPN = (evalPNTree <$>) . parsePNTree . words
    where evalPNTree (PNNumNode n) = n
          evalPNTree (PNOpNode op left right) = op (evalPNTree left) (evalPNTree right)
          parsePNTree tokenStream = do
            (tree, []) <- parsePNTree' tokenStream
            return tree
          parsePNTree' tokenStream = parsePNNumNode tokenStream <|> parsePNOpNode tokenStream
          parsePNNumNode (token:otherTokens) = do
            n <- tryParseNum token
            return (PNNumNode n, otherTokens)
          parsePNNumNode [] = Nothing
          parsePNOpNode (token:otherTokens) = do
            op <- tryGetOp token
            (left, otherTokens') <- parsePNTree' otherTokens
            (right, tokensLeft) <- parsePNTree' otherTokens'
            return (PNOpNode op left right, tokensLeft)
          parsePNOpNode [] = Nothing


-- S-Expression calculator --
-----------------------------

-- cheating : these are no real sexps
data SExp = SXNum Integer
          | SXComposite (Integer -> Integer -> Integer) [SExp]

calcSExp :: Calculator
calcSExp = (evalSExp <$>) . parseSExp . tokenize
    where tokenize = filter (/= "") . concatMap (foldr f [""]) . words
              where f '(' tokens = "":"(":tokens
                    f ')' tokens = "":")":tokens
                    f c (t:ts) = (c:t):ts
          parseSExp tokenStream = do
            (sexp, []) <- parseSExp' tokenStream
            return sexp
          parseSExp' tokenStream = parseNum tokenStream <|> parseList tokenStream
          parseNum [] = Nothing
          parseNum (token:otherTokens) = do
            n <- tryParseNum token
            return (SXNum n, otherTokens)
          parseList [] = Nothing
          parseList ("(":opToken:otherTokens) = do
            op <- tryGetOp opToken
            (listContents, tokensLeft) <- parseListContents [] otherTokens
            return (SXComposite op listContents, tokensLeft)
          parseListContents _ [] = Nothing
          parseListContents acc (")":otherTokens) = Just (reverse acc, otherTokens)
          parseListContents acc tokenStream = do
            (element, otherTokens) <- parseSExp' tokenStream
            parseListContents (element:acc) otherTokens
          evalSExp (SXNum n) = n
          evalSExp (SXComposite op sexps) = foldl1 op (map evalSExp sexps)


-- Infix notation calculator --
-------------------------------

data IntermediateRepresentationToken = IRTNum Integer
                                     -- IRTOp : binary operator as a function + priority
                                     | IRTOp (Integer -> Integer -> Integer) Integer
data ArithmeticTree = ATNumNode Integer
                    | ATOpNode (Integer -> Integer -> Integer) ArithmeticTree ArithmeticTree

calcIF str = str2ir str >>= ir2tree >>= return . evalTree
    where str2ir = str2ir' [] 0 . filter (not . isSpace)
          str2ir' [] _ "" = Nothing
          str2ir' acc _ "" = Just $ reverse acc
          str2ir' acc priorityOffset ('(':charsLeft) = str2ir' acc (priorityOffset + 5) charsLeft
          str2ir' acc priorityOffset (')':charsLeft) = str2ir' acc (priorityOffset - 5) charsLeft
          str2ir' acc priorityOffset str = do
            (tokenParsed, charsLeft) <- parseNum str <|> parseOp str priorityOffset
            str2ir' (tokenParsed:acc) priorityOffset charsLeft
          parseNum "" = Nothing
          parseNum str = case (span isDigit str) of
                           ("", _) -> Nothing
                           (numStr, charsLeft) -> tryParseNum numStr >>= \n -> Just (IRTNum n, charsLeft)
          parseOp "" _ = Nothing
          parseOp (c:cs) priorityOffset = do
            op <- tryGetOp [c]
            prio <- (+ priorityOffset) <$> lookup [c] prioAList
            return (IRTOp op prio, cs)
          prioAList = [("+", 1), ("-", 1), ("*", 2), ("/", 2)]
          ir2tree [] = Nothing
          ir2tree [IRTNum n] = Just $ ATNumNode n
          ir2tree tokenStream | not (any isIRTOp tokenStream) = Nothing
                              where isIRTOp (IRTNum _) = False
                                    isIRTOp (IRTOp _ _) = True
          ir2tree tokenStream = do
            leftTree <- ir2tree left
            rightTree <- ir2tree right
            return $ ATOpNode lowestPriorityOp leftTree rightTree
              where (_, lowestPriorityIndex, lowestPriorityOp) = foldl f (1000, -1, undefined) $ zip [0..] tokenStream
                    left = take lowestPriorityIndex tokenStream
                    right = drop (lowestPriorityIndex + 1) tokenStream
                    f acc@(currentLowestPriority, currentIndex, currentOp) (index, (IRTOp op priority)) =
                        if priority <= currentLowestPriority then (priority, index, op) else acc
                    f acc _ = acc
          evalTree (ATNumNode n) = n
          evalTree (ATOpNode op left right) = op (evalTree left) (evalTree right)

