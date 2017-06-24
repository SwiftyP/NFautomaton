module Auto (Auto, accepts, emptyA , epsA, symA  , leftA, sumA, thenA, fromLists, toLists) where

import Data.List (nub, foldl)

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }


-- NFA acceptance test
accepts:: Eq q => Auto a q -> [a] -> Bool
accepts (A s iS iA t) w = 
  any iA (nub $ foldl t_list iS w)
  where t_list [] x = []
        t_list (y:ys) x = t y x ++ t_list ys x


-- NFA accepting only the empty string
epsA:: Auto a ()
epsA =
  let s = [()]
      iS = [()]
      iA () = True
      t () x = []
  in A s iS iA t


-- NFA for empty language
emptyA:: Auto a ()
emptyA =
  let s = [()]
      iS = [()]
      iA () = False
      t () x = []
  in A s iS iA t


-- NFA for singleton language
symA:: Eq a => a -> Auto a Bool
symA a =
  let s = [False, True]
      iS = [False]
      iA False = False
      iA True = True
      t False x = if (x == a) then [True] else []
      t True x = if (x == a) then [True] else []
  in A s iS iA t


-- Same language as A
leftA:: Auto a q -> Auto a (Either q r)
leftA (A s1 iS1 iA1 t1) =
  let s = map Left s1
      iS = map Left iS1
      iA (Left q) = iA1 q
      iA (Right q) = False
      t (Right q) x = []
      t (Left q) x = map Left (t1 q x)
  in A s iS iA t


-- Union (closure under union)
sumA:: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA (A s1 iS1 iA1 t1) (A s2 iS2 iA2 t2) =
  let s = (map Left s1) ++ (map Right s2)
      iS = (map Left iS1) ++ (map Right iS2)
      iA (Left q) = iA1 q
      iA (Right q) = iA2 q
      t (Left q) x = map Left (t1 q x) -- transitions from the first NFA
      t (Right q) x = map Right (t2 q x) -- transitions from the second NFA
  in A s iS iA t


-- Concatenation (closure under concatenation)
thenA:: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA (A s1 iS1 iA1 t1) (A s2 iS2 iA2 t2) =
  let s = (map Left s1) ++ (map Right s2)
      iS = map Left iS1
      iA (Left q) = any (==True) (map iA2 iS2)
      iA (Right q) = iA2 q
      t (Right q) x = map Right (t2 q x)
      t (Left q) x | iA1 q = (map Left (t1 q x)) ++ (map Right iS2)
      t (Left q) x = map Left (t1 q x) -- for all other q and x
  in A s iS iA t


-- Translate FROM list reprezentation
fromLists:: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q, a, [q])] -> Auto a q
fromLists s1 iS1 iA1 t1 =
  let s = s1
      iS = iS1
      iA = \q -> elem q iA1
      t = \q a -> third_elem (find_in_list q a t1)
  in A s iS iA t

third_elem:: (q, a, [q]) -> [q]
third_elem (_, _, x) = x

find_in_list:: (Eq q, Eq a) => q -> a -> [(q, a, [q])] -> (q, a, [q])
find_in_list q a [] = (q, a, [])
find_in_list q a (x:xs) = if (is_this q a x) then x else find_in_list q a xs

is_this:: (Eq q, Eq a) => q -> a -> (q, a, [q])-> Bool
is_this q a (q1, a1, _) = (q == q1) && (a == a1)


-- Translate TO list reprezentation
toLists:: (Enum a, Bounded a) => Auto a q -> ([q], [q], [q], [(q, a, [q])])
toLists (A s1 iS1 iA1 t1) =
  let s = s1
      iS = iS1
      iA = [q | q <- s1, iA1 q == True]
      t = write_full (A s1 iS1 iA1 t1) s1 (enumFromTo minBound maxBound)
  in (s, iS, iA, t)

write_full:: Auto a q -> [q] -> [a] -> [(q, a, [q])]
write_full (A s1 iS1 iA1 t1) q [] = []
write_full (A s1 iS1 iA1 t1) q (a:as) = write_t_list (A s1 iS1 iA1 t1) q a ++ write_full (A s1 iS1 iA1 t1) q as

write_t_list:: Auto a q -> [q] -> a -> [(q, a, [q])]
write_t_list (A s1 iS1 iA1 t1) [] a = []
write_t_list (A s1 iS1 iA1 t1) (q:qs) a
  | not (null (t1 q a)) = [(q, a, t1 q a)] ++ write_t_list (A s1 iS1 iA1 t1) qs a
  | otherwise = write_t_list (A s1 iS1 iA1 t1) qs a









