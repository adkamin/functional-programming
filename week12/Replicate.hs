module Replicate where

-- this is the definition in the slides:
replicateM' :: (Monad m) => Int -> m a -> m [a]
replicateM' 0 _  = return []
replicateM' n mx = (:) <$> mx <*> replicateM' (n-1) mx

-- Monad instance IO String
-- Gets input 4 times, puts it in list of Strings
e1 = replicateM 4 getLine

-- Monad instance Maybe
-- Puts each Nothing inside Nothing 4 times, which is essentially Nothing
e2 = replicateM 4 Nothing

-- Monad instance Maybe
-- Replicates 4 times 37, puts it in Just list
e3 = replicateM 4 (Just 37)

-- Monad instance list
-- Gives a lits of all combinations of 4 bits
e4 = replicateM 4 [0,1]


replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM 0 _  = return []
replicateM n mx = do
    x  <- mx
    xs <- replicateM (n-1) mx
    return (x:xs)