module Utils where

import Data.Kind

iforM ::
  forall (m :: Type -> Type) (a :: Type) (b :: Type).
  (Monad m) =>
  [a] ->
  (Int -> a -> m b) ->
  m [b]
iforM list fun = go ilist
  where
    ilist = zip [0 ..] list
    go l = mapM (uncurry fun) l
