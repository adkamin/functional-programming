module MaybeMonad where

import Control.Monad

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = fmap
-- maybeMap f Nothing  = Nothing
-- maybeMap f (Just x) = pure (f x)

stripMaybe :: Maybe (Maybe a) -> Maybe a
stripMaybe = join
-- stripMaybe mmx = do { mx <- mmx ; mx}

-- stripMaybe Nothing     = Nothing
-- stripMaybe (Just x)    = x

applyMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybe f ma = ma >>= f
-- applyMaybe f ma = do { a <- ma ; f a}

-- applyMaybe f Nothing  = Nothing
-- applyMaybe f (Just x) = f x
