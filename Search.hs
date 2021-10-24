{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Array
import Data.Maybe
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node {lvl :: s, action :: Maybe a, parent :: Maybe (Node s a), depth :: Int, children :: [(Node s a)]}
                deriving (Eq, Ord)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (Node lv _ _ _ _) = lv

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ prnt _ _ )= prnt

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ dpt _ ) = dpt

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ act _ _ _) = act

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ childr) = childr

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}
mkState :: (ProblemState s a) => s -> a -> Node s a -> Int -> Node s a
mkState state act pt d = node
            where node = Node state (Just act) (Just pt) d kids
                  kids = map (\(state, act) -> mkState act state node (d + 1)) (successors state)

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace state = root
                where root = Node state Nothing Nothing 0 kids
                      kids = map (\(state, act) -> mkState act state root 1) (successors state)

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}
getStates :: [Node s a] -> [s]
getStates [] = []
getStates nodes = map (\nd -> (nodeState nd)) nodes

bfsHelper :: Ord s => [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
bfsHelper [] _ = []
bfsHelper queue vis =  if ((nodeState (head queue)) `elem` (getStates vis))
                        then bfsHelper (tail queue) vis
                        else [(nodeChildren (head queue), (tail queue) ++ (nodeChildren (head queue)))] ++ (bfsHelper ((tail queue) ++ (nodeChildren (head queue)))  (vis ++ [head queue]))
    
bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs root = bfsHelper [root] []

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

cmpAux :: Eq s => Node s a -> [Node s a] -> Maybe (Node s a, Node s a)
cmpAux _ [] = Nothing
cmpAux node (x : xs) = if (nodeState x == nodeState node) 
                    then Just (node, x)
                    else (cmpAux node xs)

cmp :: Ord s => [Node s a] -> [Node s a] -> Maybe (Node s a, Node s a)
cmp (x : xs) last = if  isNothing(cmpAux x last)
                    then (cmp xs last)
                    else (cmpAux x last)
cmp _ _ = Nothing


bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start end = fromJust (doBidir (zip (bfs start) (bfs end)))
    where
        doBidir = foldr (\(init, final) acc -> let go = (cmp (fst init) (snd final)) in
                                                        if isNothing(go)
                                                            then acc
                                                            else go) Nothing


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extrHelper :: Maybe (Node s a) -> [Maybe (Node s a)]
extrHelper node
            | isNothing(node) = []
            | otherwise       =  extrHelper (nodeParent (fromJust node)) ++ [node]  

extractPath :: Node s a -> [(Maybe a, s)]
extractPath node = map (\nd -> ((nodeAction ((fromJust) nd)), nodeState ((fromJust) nd))) $ extrHelper (Just node) 
            



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

takeAux :: (ProblemState s a, Ord s) => [(a, s)] -> [(Maybe a, s)]
takeAux [] = []
takeAux actions = map (\(act, state) -> ((Just act), state)) actions

takeFirstAction :: (ProblemState s a, Ord s) => [(Maybe a, s)]-> [(a, s)]
takeFirstAction [] = []
takeFirstAction [x] = []
takeFirstAction actions = takeFirstAction (tail actions) ++ [reverseAction ((fromJust.fst.head $ actions), snd.head.tail $ actions)]

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor


solve start end =  let pair = bidirBFS (createStateSpace start) (createStateSpace end)
                    in extractPath (fst pair) ++ (reverse (takeAux (takeFirstAction (reverse (extractPath (snd pair))))))

