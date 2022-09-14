{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = 
    let
        l1 = S.fromList ns
        l2 = S.fromList es
    in (l1, l2)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = 
    let
        l1 = S.toList (edges graph)
        rez = [b | (a, b) <- l1, node == a]
    in 
        S.fromList rez
{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = 
    let
        l1 = S.toList (edges graph)
        rez = [a | (a, b) <- l1, node == b]
    in 
        S.fromList rez
{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
remove element list = filter (\e -> e/=element) list
remove2 element list = [(a,b) | (a, b) <- list, a /= element && b /= element]

removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = 
    let
        -- lista de noduri
        l1 = S.toList (nodes graph)

        -- lista de arce
        l2 = S.toList (edges graph)

        -- sterg din lista de nodrui pe node
        rez1 = remove node l1

        --sterg din lista de arce arcele care au nodul node
        rez2 = remove2 node l2

        aux1 = S.fromList rez1
        aux2 = S.fromList rez2;
    in
       (aux1, aux2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = 
    let
        -- lista de noduri
        l1 = S.toList (nodes graph)

        -- lista de arce
        l2 = S.toList (edges graph)

        -- sterg din lista de noduri old si adaug news
        rez1 = remove old l1
        rez2 = rez1 ++ news

        -- arcele care contin nodul old
        rez3 = [(a, b) | (a,b) <- l2, a == old || b == old]
        -- aflu ce arce trebuie sa adaug in lista de arce
        nou1 = [(a, c) | (a, b) <- rez3, c <- news, a /= old]
        nou2 = [(c, b) | (a, b) <- rez3, c <- news, b /= old]
        nou = nou1 ++ nou2
        -- adaug in lista de arce noile arce create 
        l3 = l2 ++ nou
        -- sterg arcele vechi
        rez4 = remove2 old l3

        aux1 = S.fromList rez2
        aux2 = S.fromList rez4
   in
        (aux1, aux2)


{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}

mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph = 
    let
        -- lista de noduri
        l1 = S.toList (nodes graph)

        -- lista de arce
        l2 = S.toList (edges graph)

        -- construiesc noua lista de noduri

        aux = [x | x <- l1, prop x == True]
        l3 = if aux == [] then l1
            else [x | x <- l1, prop x == False] ++ [node]
        
        -- trec la arce
        rez1 = [(a, b) | (a, b) <- l2,  prop a == True || prop b == True]
        rez2 = [(a, node) | (a, b) <- rez1, prop a == False]
        rez3 = [(node, b) | (a, b) <- rez1, prop b == False]
        rez = [(node, node) | (a, b) <- rez1, prop a == True && prop b == True]
        rez4 = rez2 ++ rez3 ++ rez
        rez5 = [(a, b) | (a, b) <- rez4, prop a == False || prop b == False]
        
        -- elimin vechile arce
        rez7 = rez5 ++ rez1
        rez9 = rez7 ++ l2
        rez8 = [(a, b) | (a, b) <- rez9, prop a == False && prop b == False]
    

        aux1 = S.fromList l3
        aux2 = S.fromList rez8
    in
        (aux1, aux2)


