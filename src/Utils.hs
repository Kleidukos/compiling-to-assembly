module Utils where

import qualified Data.ByteString.Char8 as C8
import Data.Kind
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.IO (stdout)

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

say :: String -> IO ()
say = C8.hPutStrLn stdout . Text.encodeUtf8 . Text.pack
