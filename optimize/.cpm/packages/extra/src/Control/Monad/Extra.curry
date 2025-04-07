module Control.Monad.Extra where

--- Same as `concatMap`, but for a monadic function.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

--- Same as `mapM` but with an additional accumulator threaded through.
mapAccumM :: Monad m => (a -> b -> m (a, c))
          -> a -> [b] -> m (a, [c])
mapAccumM _ s []       = return (s, [])
mapAccumM f s (x : xs) = f s x >>= (\(s', x') -> (mapAccumM f s' xs) >>=
                                     (\(s'', xs') -> return (s'', x' : xs')))
