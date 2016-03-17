--Solution to assignment: http://www.cmi.ac.in/~spsuresh/teaching/prgh15/assignments/Assignment3.pdf

import Data.List(foldl')
import Data.List(nub)

--This datatype is used in first question
data BTree = Nil | Node BTree Int BTree deriving (Show)

-- inorder :: BTree -> [Int]
-- inorder Nil            = []
-- inorder (Node tl x tr) = inorder tl ++ [x] ++ inorder tr

-- preorder :: BTree -> [Int]
-- preorder Nil            = []
-- preorder (Node tl x tr) = [x] ++ preorder tl ++ preorder tr

-- postorder :: BTree -> [Int]
-- postorder Nil            = []
-- postorder (Node tl x tr) = postorder tl ++ postorder tr ++ [x]

reconstructFromInPre :: [Int] -> [Int] -> Maybe BTree
reconstructFromInPre [] []    = Just Nil
reconstructFromInPre xs ys
  | invalidArgs xs ys         = Nothing
  | otherwise                 = if helperErr then
                                  Nothing
                                else Just finalTree
  where invalidArgs :: [Int] -> [Int] -> Bool
        invalidArgs xs' ys'
          | length xs' /= length ys' = True
          -- | hasDuplicate xs'      = True
          -- | hasDuplicate ys'      = True
          | listsAreSame xs' ys'     = False  -- make sure this is the last condition as othewise returns True
          | otherwise                = True
        -- hasDuplicate (x:xs)    = x `elem` xs || hasDuplicate xs
        -- hasDuplicate []        = False
        listsAreSame (x'':xs'') ys'' = x'' `elem` ys'' && listsAreSame xs'' ys''
        listsAreSame [] _      = True
        (helperErr, finalTree) = helper xs ys
        helper :: [Int] -> [Int] -> (Bool, BTree)
        helper []  []          = (False, Nil)
        helper [i] [p]         = if i == p then
                                   (False, (Node Nil i Nil))
                                 else
                                   (True, Nil)
        helper is (p:ps)
          | invalidArgs is (p:ps) = (True, Nil)
          | otherwise             = (leftHelperErr || rightHelperErr, (Node leftHelperTree p rightHelperTree))
          where (leftHelperErr, leftHelperTree)   = helper leftis leftps
                (rightHelperErr, rightHelperTree) = helper rightis rightps 
                (leftis, rightis, _) = foldl' step1 ([], [], False) is
                step1 :: ([Int], [Int], Bool) -> Int -> ([Int], [Int], Bool) 
                step1 (left, right, False) x
                  | x == p           = (left, right, True)
                  | otherwise        = (left ++ [x], right, False)
                step1 (left, right, True) x = (left, right ++ [x], True)
                (leftps, rightps, _)    = foldl' step2 ([], [], False) ps
                step2 :: ([Int], [Int], Bool) -> Int -> ([Int], [Int], Bool)
                step2 (left, right, False) x
                  | x `elem` leftis  = (left ++ [x], right, False)
                  | otherwise        = if (x `elem` rightis) then
                                         (left, right ++ [x], True)
                                       else
                                         (left, right, False)
                step2 (left, right, True) x
                  | x `elem` rightis  = (left, right ++ [x], True)
                  | otherwise         = (left, right, True)
                                       


reconstructFromInPost :: [Int] -> [Int] -> Maybe BTree
reconstructFromInPost [] []    = Just Nil
reconstructFromInPost xs ys
  | invalidArgs xs ys         = Nothing
  | otherwise                 = if helperErr then
                                  Nothing
                                else Just finalTree
  where invalidArgs :: [Int] -> [Int] -> Bool
        invalidArgs xs' ys'
          | length xs' /= length ys' = True
          -- | hasDuplicate xs'      = True
          -- | hasDuplicate ys'      = True
          | listsAreSame xs' ys'     = False  -- make sure this is the last condition as "othewise" returns True
          | otherwise                = True
        -- hasDuplicate (x:xs)    = x `elem` xs || hasDuplicate xs
        -- hasDuplicate []        = False
        listsAreSame (x'':xs'') ys'' = x'' `elem` ys'' && listsAreSame xs'' ys''
        listsAreSame [] _      = True
        (helperErr, finalTree) = helper xs ys
        helper :: [Int] -> [Int] -> (Bool, BTree)
        helper []  []          = (False, Nil)
        helper [i] [p]         = if i == p then
                                   (False, (Node Nil i Nil))
                                 else
                                   (True, Nil)
        helper is ps
          | invalidArgs is ps = (True, Nil)
          | otherwise         = (leftHelperErr || rightHelperErr, (Node leftHelperTree (last ps) rightHelperTree))
          where (leftHelperErr, leftHelperTree)   = helper leftis leftps
                (rightHelperErr, rightHelperTree) = helper rightis rightps 
                (leftis, rightis, _) = foldl' step1 ([], [], False) is
                step1 :: ([Int], [Int], Bool) -> Int -> ([Int], [Int], Bool) 
                step1 (left, right, False) x
                  | x == (last ps)   = (left, right, True)
                  | otherwise        = (left ++ [x], right, False)
                step1 (left, right, True) x = (left, right ++ [x], True)
                (leftps, rightps, _)    = foldl' step2 ([], [], False) ps
                step2 :: ([Int], [Int], Bool) -> Int -> ([Int], [Int], Bool)
                step2 (left, right, False) x
                  | x `elem` leftis  = (left ++ [x], right, False)
                  | otherwise        = if (x `elem` rightis) then
                                         (left, right ++ [x], True)
                                       else
                                         (left, right, False)
                step2 (left, right, True) x
                  | x `elem` rightis  = (left, right ++ [x], True)
                  | otherwise         = (left, right, True)

--This datatype is used in second question
data Color    = Red | Black deriving (Eq, Ord, Show)
data RBTree a = RBNil | RBNode (RBTree a) a Color (RBTree a) deriving (Eq, Ord, Show)

isRedBlack :: Ord a => RBTree a -> Bool
isRedBlack RBNil = True
isRedBlack tree
  | notSearchTree tree   = False 
  | noRedNodeChild tree  = False
  | noBlackNodePath tree = False
  | otherwise            = True
  where notSearchTree RBNil              = False
        notSearchTree (RBNode tl x c tr) = (checkLeft tl x) || (checkRight tr x) || (notSearchTree tl) || (notSearchTree tr)
          where checkLeft RBNil _             = False
                checkLeft (RBNode tl' y _ tr') x  = (x < y) || (checkLeft tl' x) || (checkLeft tr' x)
                checkRight RBNil _= False
                checkRight (RBNode tl' y _ tr') x = (x >= y) || (checkRight tl' x) || (checkRight tr' x)
        noRedNodeChild RBNil                  = False
        noRedNodeChild (RBNode tl _ Red tr)   = (checkLeft' tl) || (checkRight' tr) || (noRedNodeChild tl) || (noRedNodeChild tr)
          where checkLeft' RBNil              = False
                checkLeft' (RBNode _ _ c _)   = (c == Red)
                checkRight' RBNil             = False
                checkRight' (RBNode _ _ c _)  = (c == Red) 
        noRedNodeChild (RBNode tl _ Black tr) = (noRedNodeChild tl) || (noRedNodeChild tr)
        noBlackNodePath RBNil                 = False
        noBlackNodePath tree                  = not (length (nub (listOfBlackNodePath tree)) == 1)
          where listOfBlackNodePath (RBNode tl _ Black tr) = (listOfBlackNodePath' tl 1) ++ (listOfBlackNodePath' tr  1)
                listOfBlackNodePath (RBNode tl _ Red tr)   = (listOfBlackNodePath' tl 0) ++ (listOfBlackNodePath' tr  0)
                listOfBlackNodePath' RBNil curCount                  = [curCount+1]
                -- listOfBlackNodePath' (RBNode RBNil _ Black RBNil) curCount    = [curCount+1]
                -- listOfBlackNodePath' (RBNode RBNil _ Red RBNil)   curCount    = [curCount]
                listOfBlackNodePath' (RBNode tl _ Black tr) curCount = (listOfBlackNodePath' tl (curCount + 1)) ++ (listOfBlackNodePath' tr (curCount + 1))
                listOfBlackNodePath' (RBNode tl _ Red tr) curCount   = (listOfBlackNodePath' tl curCount) ++ (listOfBlackNodePath' tr curCount)

--assumption here is that given tree is a red black tree (hence a binary search tree)
member :: Ord a => RBTree a -> a -> Bool
member RBNil _                    = False
member (RBNode RBNil x _ RBNil) k = (x == k)
member (RBNode lt x _ rt) k
  | x == k                        = True
  | x < k                         = member rt k
  | otherwise                     = member lt k


infList :: [Integer]
infList = 1 : myMerge (myMerge [2*x | x <- infList] [3*x | x <- infList]) [5*x | x <- infList]
  where myMerge (x:xs) (y:ys)
          | x < y  = x : myMerge xs (y:ys)
          | x == y = x : myMerge xs ys
          | x > y  = y : myMerge (x:xs) ys
                           
