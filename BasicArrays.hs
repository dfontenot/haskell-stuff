-- http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/arrays.html

module BasicArrays where
import Prelude
import Data.Array
import Control.Monad

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

-- iterative method
matPower :: (Num a) => Array (Int, Int) a -> Int -> Maybe (Array (Int, Int) a)
matPower m i = matPower' (Just m) (Just m) i
         where
         matPower' maccum _ i | i == 1 = maccum
         matPower' maccum m i | otherwise = matPower' (join $ liftM2 matMult maccum m) m (i-1)

-- get the diagonal row of the identity matrix
diagOfId4 = (liftM getDiag) (matMult (idMatrix 4) (idMatrix 4))

-- basic arrays
-- TODO: use some more diverse matricies to check results
mat1 = array ((1,1),(2,2)) [((1,1),1), ((1,2),2), ((2,1),3), ((2,2),4)]
mat2 = array ((1,1),(2,2)) [((1,1),5), ((1,2),6), ((2,1),7), ((2,2),8)]

-- answers
multAns = Just (array ((1,1),(2,2)) [((1,1),19), ((1,2),22), ((2,1),43), ((2,2),50)])
mat1SquaredAns = Just (array ((1,1),(2,2)) [((1,1),7), ((1,2),10), ((2,1),15), ((2,2),22)])
mat2QuintedAns = Just (array ((1,1),(2,2)) [((1,1),152393), ((1,2),177474), ((2,1),207053), ((2,2),241130)])

-- operations
matrixproduct = matMult mat1 mat2
mat1Squared = matPower mat1 2
mat2Quinted = matPower mat2 5

-- basic check to see if the math is right
multCorrect = multAns == matrixproduct
powerCorrect = mat1Squared == mat1SquaredAns && mat2Quinted == mat2QuintedAns && (matPower mat1 1) == (Just mat1)