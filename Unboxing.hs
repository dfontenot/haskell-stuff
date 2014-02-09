module UnBoxing where
import Prelude
import Data.Maybe
import Control.Monad
import Control.Applicative

maybeVal = Just 5
noVal = Nothing

maybeLift = liftM (+2) maybeVal
noLift = liftM (+2) noVal

fmapped = fmap (+2) maybeVal
noFmapped = fmap (+2) noVal

boundFmap = maybeLift >>= return . (+2)

combinedMaybes = Just (+2) <*> maybeLift
noCombined = Just (+3) <*> noLift

apLift = Just (+2) `ap` maybeLift

fmapApplicative = (+2) <$> maybeVal

ages :: [(String, Int)]
ages = [("David", 23), ("Aaron", 21)]

maybeDavid = lookup "David" ages

davidBirthday = liftM (+1) $ lookup "David" ages

aaronOlder = liftA2 (<) (lookup "David" ages) (lookup "Aaron" ages)
johnDoeOlder = liftA2 (<) (lookup "David" ages) (lookup "John Doe" ages)

-- return puts a value into a monad
usingReturn = (return "David") :: Maybe String
