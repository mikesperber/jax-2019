data Board = Board Position Position Position Position -- ul, ur, ll, lr

instance Show Board where
  show (Board ul ur ll lr) = (show ul) ++ " | " ++ (show ur) ++ "\n-----\n" ++ (show ll) ++ " | " ++ (show lr)

listToBoard [ul, ur, ll, lr] = Board ul ur ll lr
boardToList (Board ul ur ll lr) = [ul, ur, ll, lr]

data Position = X | O | Empty
  deriving Eq

instance Show Position where
  show X = "X"
  show O = "O"
  show Empty = " "

moves :: Board -> [Board]
moves b = map listToBoard (moves' (boardToList b))

linePositions :: Board -> [[Position]]
linePositions (Board ul ur ll lr) =
  [[ul, ur], [ll, lr], [ul, ll], [ur, lr], [ul, lr], [ur, ll]]

won :: Board -> Position -> Bool
won board pos =
  any (all ((==) pos)) (linePositions board)

nextPlayer :: Board -> Position
nextPlayer board =
  let lis = boardToList board
      xs = filter ((==) X) lis
      os = filter ((==) O) lis
  in if length xs <= length os
     then X
     else O

moves' :: [Position] -> [[Position]]
moves' [] = []
moves' (Empty : rest) = [X : rest, O : rest] ++ (map ((:) Empty) (moves' rest))
moves' (p : rest) = map ((:) p) (moves' rest)

data Tree a = Node a [Tree a]

maptree f (Node a sub) = Node (f a) (map (maptree f) sub)

instance Functor Tree where
  fmap = maptree

reptree :: (a -> [a]) -> a -> Tree a
reptree f a = Node a (map (reptree f) (f a))

gametree :: Board -> Tree Board
gametree p = reptree moves p

static :: Board -> Int
static board = if won board X
               then 1
               else if won board O
               then -1
               else 0

maximize :: Tree Int -> Int
maximize (Node n []) = n
maximize (Node n sub) = maximum (map minimize sub)
minimize :: Tree Int -> Int
minimize (Node n []) = n
minimize (Node n sub) = minimum (map maximize sub)

{-
maximize :: Tree Int -> Int
maximize = maximum . maximize'
maximize' :: Tree Int -> [Int]
maximize' (Node n []) = [n]
maximize' (Node n l) =
  -- = map minimize l
  -- = map (min . minimize') l
  -- = map min (map minimize' l)
  mapmin (map minimize' l)
mapmin :: [[Int]] -> [Int]
-- mapmin = map minimum
mapmin (nums:rest) = (minimum nums) : (omit (minimum nums) rest)

minimize :: Tree Int -> Int
minimize = minimum . minimize'
minimize' :: Tree Int -> [Int]
minimize' (Node n []) = [n]
minimize' (Node n l) =
  -- = map maximize l
  -- = map (max . maximize') l
  -- = map max (map maximize' l)
  mapmax (map maximize' l)
mapmax = map maximum
-- mapmax (nums:rest) = (maximum nums) : (omit (maximum nums) rest)
-}

prune 0 (Node a x) = Node a []
prune n (Node a x) = Node a (map (prune (n - 1)) x)

evaluate0 = maximize . fmap static . gametree

-- prune a . gametree can't be modularized without lazy evaluation
-- also, the tree is only constructed as maximize requires it
evaluate = maximize . fmap static . prune 5 . gametree
