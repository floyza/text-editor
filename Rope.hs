-- Rope.hs
--
-- TODO: balance tree after concat and split

module Rope where

data Rope = Node Rope Rope
            | Tip [Char] deriving (Show)

example = Node (Node (Tip "Hello_") (Tip "my_"))
               (Node (Node (Tip "na") (Tip "me_i"))
                     (Node (Tip "s") (Tip "_Simon")))

ropeToStr :: Rope -> [Char]
ropeToStr s = map (index s) [0..(weight s)-1]

-- TODO: store in Node type instead of calculating on the fly
weight :: Rope -> Int
weight (Node lhs rhs) = weight lhs + weight rhs
weight (Tip s) = length s

-- get char from index
index :: Rope -> Int -> Char
index (Node lhs rhs) i =
  if (weight lhs <= i)
  then index rhs (i - weight lhs) -- right branch
  else index lhs i -- left branch
index (Tip s) i = s !! i

rconcat :: Rope -> Rope -> Rope
rconcat = Node

-- split string by index
split :: Rope -> Int -> (Rope, Rope)
split (Node lhs rhs) i
  | i == weight lhs = (lhs, rhs) -- perfect split inbetween nodes
  | (weight lhs <= i) = let new = split rhs (i - weight lhs) -- right branch
    in (rconcat lhs (fst new), snd new) -- fst gets left branch
  | otherwise = let new = split lhs i -- left branch
    in (fst new, rconcat (snd new) rhs) -- snd gets right branch
split (Tip s) i = (Tip (take i s), Tip (drop i s)) -- need to split a node

-- insert s' at index i into s
insert :: Rope -> Int -> Rope -> Rope
insert s i s' = let r = split s i
                in (rconcat (fst r) (rconcat s' (snd r)))

-- delete substring s[i] to s[i+j]
delete :: Rope -> Int -> Int -> Rope
delete s i j = rconcat (fst (split s i)) (snd (split s (i+j)))

-- return substring s[i] to s[i+j]
-- TODO: is this inefficient? recommended implementation by wikipedia:
-- To report the string C[i], …, C[i + j − 1], find the node u that contains C[i] and weight(u) >= j, and then traverse T starting at node u. Output C[i], …, C[i + j − 1] by doing an in-order traversal of T starting at node u.
-- with link from in-order traversal: https://en.wikipedia.org/wiki/Tree_traversal#In-order
report :: Rope -> Int -> Int -> Rope
report s i j = snd (split (fst (split s (i+j))) i)
