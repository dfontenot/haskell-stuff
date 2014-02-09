-- http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/arrays.html

module BasicArrays where
import Prelude
import Data.Array

-- array (lowbound, highbound) [(idx, val)]
squares = array (1,100) [(i, i*i) | i <- [1..100]]

-- array ((low1, low2),(high1,high2)) [((x,y),val)]
twoDimen = array ((1,1),(4,4)) [((i,j),1) | i <- [1..4], j <- [1..4]]

idMatrix :: Int -> Array (Int, Int) Int
idMatrix sz = array ((1,1),(sz,sz)) 
         [((i,j), if i == j then 1 else 0) | i <- [1..sz], j <- [1..sz]]

-- get elem at location 1,1
element = (idMatrix 4) ! (1, 1)

assocsVals :: (Num a) => [((Int,Int),a)] -> [a]
assocsVals lst = map (\((_,_),v) -> v) lst

selectElems :: (Num a) => Array (Int, Int) a -> (((Int,Int),a) -> Bool) -> [a]
selectElems a fnc = assocsVals $ filter fnc $ assocs a

getRow :: (Num a) => Array (Int, Int) a -> Int -> [a]
getRow a i = selectElems a (\((x,_),_) -> x == i)

getCol :: (Num a) => Array (Int, Int) a -> Int -> [a]
getCol a i = selectElems a (\((_,x),_) -> x == i)

getDiag :: (Num a) => Array (Int, Int) a -> [a]
getDiag a = selectElems a (\((x,y),_) -> x == y)

dot :: (Num a) => [a] -> [a] -> a
dot a b = foldl (+) 0 $ zipWith (*) a b

matMult :: (Num a) => Array (Int, Int) a -> Array (Int, Int) a -> Maybe (Array (Int, Int) a)
matMult a b = if n1 /= n2 then Nothing else Just $ array ((1,1),(r,c)) 
                                                         [((i,j), rowByCol' a b i j) | i <- [1..r], j <- [1..c]]
        where
        (_,(r,n1)) = bounds a
        (_,(n2,c)) = bounds b
        rowByCol' a b i j = dot (getRow a i) (getCol b j)
