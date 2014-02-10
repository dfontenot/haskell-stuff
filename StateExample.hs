module StateExample where
import Control.Monad.State

-- a simple DFA that excepts (ab|c)*
acceptingStates = [1,3]
transitions = [(1,'c',1), (1,'a',2), (2,'b',3), (3,'a',2), (3,'c',1)]
liftedTrans = map (\(s,c,t) -> (Just s, c, Just t)) transitions

type DFAState = Maybe Integer
type DFAValue = Maybe Integer


-- TODO: refactor, very ugly
getTransition :: DFAState -> Char -> DFAState
getTransition Nothing _ = Nothing
getTransition curState c = getTrans' curState c liftedTrans
    where
    getTrans' _ _ [] = Nothing
    getTrans' curSt ch ((s,cha,t):xs) = case curSt == s && ch == cha of
                                        True -> t
                                        False -> getTrans' curSt ch xs

-- TODO: refactor, also ugly
accepts :: String -> State DFAState DFAValue
accepts [] = do
        finalState <- get
        let accepting = map (\i -> Just i) acceptingStates in
            return $ case finalState `elem` accepting of
                     True -> finalState
                     False -> Nothing
accepts (x:xs) = do
        curState <- get
        put $ getTransition curState x
        accepts xs

startState = Just 1

main :: IO ()
main = do
     case evalState (accepts "abababccababcab") startState of
          Just i -> putStrLn $ "String accepted, ended in state " ++ show i
          Nothing -> putStrLn "String not accepted"
