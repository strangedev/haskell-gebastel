import Data.Maybe

data BinaryTree a = Node {
  value :: a,
  height :: Int, 
  left :: (Maybe (BinaryTree a)),
  right :: (Maybe (BinaryTree a))
} 

hasLeft :: (Ord a, Eq a) => BinaryTree a -> Bool
hasLeft Node{left=l} = isJust l

hasRight :: (Ord a, Eq a) => BinaryTree a -> Bool
hasRight Node{right=r} = isJust r

justLeft :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
justLeft Node{left=l} = fromJust l

justRight :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
justRight Node{right=r} = fromJust r

isLeaf :: (Ord a, Eq a) => BinaryTree a -> Bool
isLeaf tree = not (hasLeft tree || hasRight tree)

wrapIn :: String -> String -> String -> String
wrapIn leftDelimiter rightDelimiter x = leftDelimiter ++ x ++ rightDelimiter

wrapInParens = wrapIn "( " " )"
wrapInBrackts = wrapIn "[ " " ]"
wrapInAngleBrackets = wrapIn "< " " >"

instance (Show a) => Show (BinaryTree a) where
  show tree =
    let
      showValue isRoot a h = case isRoot of
        True -> wrapInAngleBrackets $ show a ++ ":" ++ show h
        False -> show a ++ ":" ++ show h
      showTree isRoot (Node a h Nothing Nothing) = wrapInBrackts $ showValue isRoot a h 
      showTree isRoot (Node a h (Just left) Nothing) = wrapInParens $ (showTree False left) ++ " <- " ++ (showValue isRoot a h)
      showTree isRoot (Node a h Nothing (Just right)) = wrapInParens $ (showValue isRoot a h) ++ " -> " ++ (showTree False right)
      showTree isRoot (Node a h (Just left) (Just right)) = wrapInParens $ (showTree False left) ++ " <- " ++ (showValue isRoot a h) ++ " -> " ++ (showTree False right)
    in showTree True tree

{-
height :: (Ord a, Eq a) => BinaryTree a -> Int
height tree = treeHeight 0 tree
  where treeHeight h (Node _ Nothing Nothing) = h
        treeHeight h (Node _ (Just left) Nothing)   = treeHeight (h + 1) left
        treeHeight h (Node _ Nothing (Just right))  = treeHeight (h + 1) right
        treeHeight h tree = let leftHeight = treeHeight (h + 1) (justLeft tree)
                                rightHeight = treeHeight (h + 1) (justRight tree)
                            in if leftHeight > rightHeight 
                              then leftHeight
                              else rightHeight
-}

balance :: (Ord a, Eq a) => BinaryTree a -> Int
balance tree = case (left tree, right tree) of
  (Nothing, Nothing)    -> 0
  ((Just l), Nothing)   -> - (height l)
  (Nothing, (Just r))   -> height r
  ((Just l), (Just r))  -> (height . justRight $ tree) - (height . justLeft $ tree)

unbalancedInsert :: (Ord a, Eq a) => Maybe (BinaryTree a) -> a -> BinaryTree a
unbalancedInsert Nothing valueToInsert = Node valueToInsert 0 Nothing Nothing
unbalancedInsert (Just Node{value=v, left=l, right=r}) valueToInsert = if valueToInsert <= v
  then
    let newLeft = unbalancedInsert l valueToInsert
        rightHeight = case r of
          Nothing       -> -1
          Just someTree -> height someTree          
    in Node v ((max (height newLeft) rightHeight) + 1) (Just newLeft) r 
  else
    let newRight = unbalancedInsert r valueToInsert
        leftHeight = case l of
          Nothing       -> -1
          Just someTree -> height someTree          
    in Node v ((max leftHeight (height newRight)) + 1) l (Just newRight)

chainUnbalancedInsert :: (Ord a, Eq a) => Maybe (BinaryTree a) -> a -> Maybe (BinaryTree a)
chainUnbalancedInsert tree a = Just $ unbalancedInsert tree a

unbalancedInsertMany :: (Ord a, Eq a) => Maybe (BinaryTree a) -> [a] -> Maybe (BinaryTree a)
unbalancedInsertMany tree [] = tree
unbalancedInsertMany tree (x:xs) = unbalancedInsertMany (chainUnbalancedInsert tree x) xs

{-
unbalancedDelete :: (Ord a, Eq a) => Maybe (BinaryTree a) -> a -> Maybe (BinaryTree a)
unbalancedDelete Nothing _ = Nothing
unbalancedDelete (Just (Node value left right)) valueToDelete = case compare valueToDelete value of
  LT -> Just $ Node value (unbalancedDelete left valueToDelete) right
  EQ -> deleteRoot (Node value left right)
  GT -> Just $ Node value left (unbalancedDelete right valueToDelete)

popLeftmost :: (Ord a, Eq a) => BinaryTree a -> (Maybe (BinaryTree a), a)
popLeftmost (Node value Nothing right) = (right, value)
popLeftmost (Node value (Just left) right) = (Just poppedTree, poppedValue)
  where (poppedLeftSubtree, poppedValue) = popLeftmost left
        poppedTree = Node value poppedLeftSubtree right 

popRightmost :: (Ord a, Eq a) => BinaryTree a -> (Maybe (BinaryTree a), a)
popRightmost (Node value left Nothing) = (left, value)
popRightmost (Node value left (Just right)) = (Just poppedTree, poppedValue)
  where (poppedRightSubtree, poppedValue) = popRightmost right
        poppedTree = Node value left poppedRightSubtree

deleteRoot :: (Ord a, Eq a) => BinaryTree a -> Maybe (BinaryTree a)
deleteRoot root
  | isLeaf root = Nothing
  | hasLeft root =  let (newLeft, newRootValue) = popRightmost . justLeft $ root in
    Just $ Node newRootValue (newLeft) (right root)
  | hasRight root = let (newRight, newRootValue) = popLeftmost . justRight $ root in
    Just $ Node newRootValue (left root) newRight

{-
leftRotate :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
leftRotate (Node value left Nothing) = Node value left Nothing
leftRotate (Node rootValue leftTree (Just rightTree)) = Node (value rightTree) (Just leftOfRoot) (right rightTree)
  where leftOfRoot = Node rootValue leftTree (left rightTree)

rightRotate :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
rightRotate (Node value Nothing right) = Node value Nothing right
rightRotate (Node rootValue (Just leftTree) rightTree) = Node (value leftTree) (left leftTree) (Just rightOfRoot)
  where rightOfRoot = Node rootValue (right leftTree) rightTree

data Balancedness = Balanced | RightHeavy | LeftHeavy
  deriving Show

classifyBalancedness :: (Ord a, Eq a) => BinaryTree a -> Balancedness
classifyBalancedness tree
  | treeBalance < -1 = LeftHeavy
  | treeBalance > 1 = RightHeavy
  | otherwise = Balanced
  where treeBalance = balance tree

avlRebalance :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
avlRebalance tree = case classifyBalancedness tree of
  Balanced -> tree
  RightHeavy -> let rightTree = justRight tree in
    case classifyBalancedness rightTree of
      Balanced -> leftRotate tree
      RightHeavy -> leftRotate tree
      LeftHeavy -> leftRotate (Node (value tree) (left tree) (Just $ rightRotate  rightTree))
  LeftHeavy -> let leftTree = justLeft tree in
    case classifyBalancedness leftTree of
      Balanced -> rightRotate tree
      LeftHeavy -> rightRotate tree
      RightHeavy -> rightRotate (Node (value tree) (Just $ leftRotate leftTree) (right tree))

avlInsert :: (Ord a, Eq a) => Maybe (BinaryTree a) -> a -> BinaryTree a
avlInsert Nothing valueToInsert = unbalancedInsert Nothing valueToInsert 
avlInsert (Just (Node value left right)) valueToInsert = case compare valueToInsert value of
  LT -> avlRebalance $ Node value (Just $ avlInsert left valueToInsert) right
  EQ -> avlRebalance $ Node value (Just $ avlInsert left valueToInsert) right
  GT -> avlRebalance $ Node value left (Just $ avlInsert right valueToInsert)

avlDelete :: (Ord a, Eq a) => Maybe (BinaryTree a) -> a -> Maybe (BinaryTree a)
avlDelete Nothing _ = Nothing
avlDelete (Just (Node value left right)) valueToDelete = case compare valueToDelete value of
  LT -> Just $ avlRebalance $ Node value (avlDelete left valueToDelete) right
  EQ -> fmap avlRebalance $ deleteRoot (Node value left right)
  GT -> Just $ avlRebalance $ Node value left (avlDelete right valueToDelete)

a0 = avlInsert Nothing 10
a1 = avlInsert (Just a0) 9
a2 = avlInsert (Just a1) 8
a3 = avlInsert (Just a2) 7
a4 = avlInsert (Just a3) 6
a5 = avlInsert (Just a4) 5
a6 = avlInsert (Just a5) 4
a7 = avlInsert (Just a6) 3
a6d = avlDelete (Just a7) 9
a5d = avlDelete (a6d) 8
a4d = avlDelete (a5d) 7
a3d = avlDelete (a4d) 6
a2d = avlDelete (a3d) 5
a1d = avlDelete (a2d) 4
a0d = avlDelete (a1d) 3

main = do
  print a0
  print a1
  print a2
  print a3
  print a4
  print a5
  print a6
  print a7
  print a6d
  print a5d
  print a4d
  print a3d
  print a2d
  print a1d
  print a0d
-}