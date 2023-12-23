module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Напишите тесты к функции и функцию
--
-- Работает как zip, но если один список
-- длиннее, циклически переиспользует второй
zipLong :: [a] -> [b] -> [(a, b)]
zipLong [] _ = []
zipLong _ [] = []
zipLong as bs
  | length as >= length bs = zipWith f as (cycle bs)
  | otherwise = zipWith f (cycle as) bs
  where
    f a b = (a,b)


-- Binary Search Tree
--
-- left < root <= right
data Tree a
  = Empty
  | Node
    { left :: Maybe (Tree a)
    , value :: a
    , right :: Maybe (Tree a)
    }
  deriving (Eq,Show,Read)

empty :: Tree a
empty = Empty

leaf :: a -> Tree a
leaf a = Node Nothing a Nothing

traversal :: Tree a -> [a]
traversal Empty = []
traversal (Node ml v mr)
  = maybe [] traversal ml ++ [v] ++ maybe [] traversal mr

insert :: Ord a => a -> Tree a -> Tree a
insert v Empty = leaf v
insert v t@(Node ml root mr)
  | v < root  = t{ left = Just $ maybe (leaf v) (insert v) ml }
  | otherwise = t{ right= Just $ maybe (leaf v) (insert v) mr }

--makeNode :: Maybe (Tree a) -> a -> Maybe (Tree a) -> Tree a
--makeNode l v r = Node l v r

-- Напишите тесты-свойства к функциям и сами функции
-- левого и правого поворота деревьев
-- (см. https://en.wikipedia.org/wiki/Red%E2%80%93black_tree)
rotateLeft :: Tree a -> Tree a
rotateLeft (Node  l v (Just (Node rl rv rr)))
  = Node (Just $ Node l v rl) rv rr
rotateLeft t = t

rotateRight :: Tree a -> Tree a
rotateRight (Node (Just (Node ll lv lr)) v r)
  = Node ll lv (Just (Node lr v r))
rotateRight t = t
