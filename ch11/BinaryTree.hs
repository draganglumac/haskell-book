module BinaryTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' a Leaf = Node Leaf a Leaf
insert' a (Node left b right)
  | a < b  = Node (insert' a left) b right
  | a == b = Node left b right
  | a > b  = Node left b (insert' a right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f t = case t of
              Leaf -> Leaf
              Node l a r -> Node (mapTree f l) (f a) (mapTree f r)

preorder :: BinaryTree a -> [a]
preorder t = case t of
             Leaf -> []
             Node l a r -> [a] ++ preorder l ++ preorder r

inorder :: BinaryTree a -> [a]
inorder t = case t of
            Leaf -> []
            Node l a r -> inorder l ++ [a] ++ inorder r

postorder :: BinaryTree a -> [a]
postorder t = case t of
              Leaf -> []
              Node l a r -> postorder l ++ postorder r ++ [a]

-- any traversal is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b t = case t of
              Leaf -> b
              Node l a r -> foldTree f (f a (foldTree f b l)) r
