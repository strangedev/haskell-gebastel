import Data.Maybe

data BinyaryTree a = Node {
 value :: a, 
 left :: (Maybe (BinyaryTree a)),
 right :: (Maybe (BinyaryTree a))
} 

hasLeft :: (Ord a, Eq a) => BinyaryTree a -> Bool
hasLeft (Node _ left _) = isJust left

hasRight :: (Ord a, Eq a) => BinyaryTree a -> Bool
hasRight (Node _ _ right) = isJust right

justLeft :: (Ord a, Eq a) => BinyaryTree a -> BinyaryTree a
justLeft (Node _ left _) = fromJust left

justRight :: (Ord a, Eq a) => BinyaryTree a -> BinyaryTree a
justRight (Node _ _ right) = fromJust right

isLeaf :: (Ord a, Eq a) => BinyaryTree a -> Bool
isLeaf tree = not (hasLeft tree || hasRight tree)

height :: (Ord a, Eq a) => BinyaryTree a -> Int
height tree = treeHeight 0 tree
  where treeHeight h (Node _ Nothing Nothing) = h
        treeHeight h (Node _ (Just left) Nothing)   = treeHeight (h + 1) left
        treeHeight h (Node _ Nothing (Just right))  = treeHeight (h + 1) right
        treeHeight h tree = let leftHeight = treeHeight (h + 1) (justLeft tree)
                                rightHeight = treeHeight (h + 1) (justRight tree)
                            in if leftHeight > rightHeight 
                              then leftHeight
                              else rightHeight

balance :: (Ord a, Eq a) => BinyaryTree a -> Int
balance (Node _ Nothing Nothing) = 0
balance (Node _ (Just left) Nothing) = 0 - (height left)
balance (Node _ Nothing (Just right)) = height right
balance tree = (height . justRight $ tree) - (height . justLeft $ tree)

unbalancedInsert :: (Ord a, Eq a) => Maybe (BinyaryTree a) -> a -> BinyaryTree a
unbalancedInsert Nothing valueToInsert = Node valueToInsert Nothing Nothing
unbalancedInsert (Just (Node value left right)) valueToInsert = case compare valueToInsert value of
  LT -> Node value (Just $ unbalancedInsert left valueToInsert) right
  EQ -> Node value (Just $ unbalancedInsert left valueToInsert) right
  GT -> Node value left (Just $ unbalancedInsert right valueToInsert)

chainUnbalancedInsert :: (Ord a, Eq a) => Maybe (BinyaryTree a) -> a -> Maybe (BinyaryTree a)
chainUnbalancedInsert tree a = Just $ unbalancedInsert tree a

unbalancedInsertMany :: (Ord a, Eq a) => Maybe (BinyaryTree a) -> [a] -> Maybe (BinyaryTree a)
unbalancedInsertMany tree [] = tree
unbalancedInsertMany tree (x:xs) = unbalancedInsertMany (chainUnbalancedInsert tree x) xs

unbalancedDelete :: (Ord a, Eq a) => Maybe (BinyaryTree a) -> a -> Maybe (BinyaryTree a)
unbalancedDelete Nothing _ = Nothing
unbalancedDelete (Just (Node value left right)) valueToDelete = case compare valueToDelete value of
  LT -> Just $ Node value (unbalancedDelete left valueToDelete) right
  EQ -> deleteRoot (Node value left right)
  GT -> Just $ Node value left (unbalancedDelete right valueToDelete)

popLeftmost :: (Ord a, Eq a) => BinyaryTree a -> (Maybe (BinyaryTree a), a)
popLeftmost (Node value Nothing right) = (right, value)
popLeftmost (Node value (Just left) right) = (Just poppedTree, poppedValue)
  where (poppedLeftSubtree, poppedValue) = popLeftmost left
        poppedTree = Node value poppedLeftSubtree right 

popRightmost :: (Ord a, Eq a) => BinyaryTree a -> (Maybe (BinyaryTree a), a)
popRightmost (Node value left Nothing) = (left, value)
popRightmost (Node value left (Just right)) = (Just poppedTree, poppedValue)
  where (poppedRightSubtree, poppedValue) = popRightmost right
        poppedTree = Node value left poppedRightSubtree

deleteRoot :: (Ord a, Eq a) => BinyaryTree a -> Maybe (BinyaryTree a)
deleteRoot root
  | isLeaf root = Nothing
  | hasLeft root =  let (newLeft, newRootValue) = popRightmost . justLeft $ root in
    Just $ Node newRootValue (newLeft) (right root)
  | hasRight root = let (newRight, newRootValue) = popLeftmost . justRight $ root in
    Just $ Node newRootValue (left root) newRight

wrapIn :: String -> String -> String -> String
wrapIn leftDelimiter rightDelimiter x = leftDelimiter ++ x ++ rightDelimiter

wrapInParens = wrapIn "( " " )"
wrapInBrackts = wrapIn "[ " " ]"

instance (Show a) => Show (BinyaryTree a) where
  show (Node a Nothing Nothing) = wrapInBrackts $ show a
  show (Node a (Just left) Nothing) = wrapInParens $ show left ++ " <- " ++ show a
  show (Node a Nothing (Just right)) = wrapInParens $ show a ++ " -> " ++ show right
  show (Node a (Just left) (Just right)) = wrapInParens $ show left ++ " <- " ++ show a ++ " -> " ++ show right


