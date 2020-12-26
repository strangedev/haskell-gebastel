{- This program was written to practive file IO. -}
import Data.Maybe

data BinyaryTree a = Node a (Maybe (BinyaryTree a)) (Maybe (BinyaryTree a))
  deriving Show

hasLeft :: (Ord a) => BinyaryTree a -> Bool
hasLeft (Node _ left _) = isJust left

hasRight :: (Ord a) => BinyaryTree a -> Bool
hasRight (Node _ _ right) = isJust right

justLeft :: (Ord a) => BinyaryTree a -> BinyaryTree a
justLeft (Node _ left _) = fromJust left

justRight :: (Ord a) => BinyaryTree a -> BinyaryTree a
justRight (Node _ _ right) = fromJust right

getValue :: (Ord a) => BinyaryTree a -> a
getValue (Node a _ _) = a

height :: (Ord a) => BinyaryTree a -> Int
height tree = treeHeight 0 tree
  where treeHeight h (Node _ Nothing Nothing) = h
        treeHeight h (Node _ (Just left) Nothing)   = treeHeight (h + 1) left
        treeHeight h (Node _ Nothing (Just right))  = treeHeight (h + 1) right
        treeHeight h tree                           = let leftHeight = treeHeight (h + 1) (justLeft tree)
                                                          rightHeight = treeHeight (h + 1) (justRight tree)
                                                      in if leftHeight > rightHeight 
                                                        then leftHeight
                                                        else rightHeight

balance :: (Ord a) => BinyaryTree a -> Int
balance (Node _ Nothing Nothing) = 0
balance (Node _ (Just left) Nothing) = 0 - (height left)
balance (Node _ Nothing (Just right)) = height right
balance tree = (height . justRight $ tree) - (height . justLeft $ tree)

unbalancedInsert :: (Ord a) => Maybe (BinyaryTree a) -> a -> BinyaryTree a
unbalancedInsert Nothing valueToInsert = Node valueToInsert Nothing Nothing
unbalancedInsert (Just (Node value left right)) valueToInsert = case compare valueToInsert value of
  LT -> Node value (Just $ unbalancedInsert left valueToInsert) right
  EQ -> Node value (Just $ unbalancedInsert left valueToInsert) right
  GT -> Node value left (Just $ unbalancedInsert right valueToInsert)

chainUnbalancedInsert :: (Ord a) => Maybe (BinyaryTree a) -> a -> Maybe (BinyaryTree a)
chainUnbalancedInsert tree a = Just $ unbalancedInsert tree a

unbalancedInsertMany :: (Ord a) => Maybe (BinyaryTree a) -> [a] -> Maybe (BinyaryTree a)
unbalancedInsertMany tree [] = tree
unbalancedInsertMany tree (x:xs) = unbalancedInsertMany (chainUnbalancedInsert tree x) xs

unbalancedDelete :: (Ord a) => Maybe (BinyaryTree a) -> a -> Maybe (BinyaryTree a)
unbalancedDelete Nothing _ = Nothing
unbalancedDelete (Just (Node value left right)) valueToDelete = case compare valueToDelete value of
  LT -> Just $ Node value (unbalancedDelete left valueToDelete) right
  EQ -> Nothing -- TODO
  GT -> Just $ Node value left (unbalancedDelete right valueToDelete)
