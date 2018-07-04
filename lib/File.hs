module File where

import "rio" RIO

import "freer-simple" Control.Monad.Freer (Eff, Member, send)
import "path-io" Path.IO                  (makeAbsolute, resolveFile')
import "rio" RIO.Text                     (unpack)

import qualified "path" Path

import qualified "this" Env

data File a where
  Absolute :: !Env.PurtyFilePath -> File (Path.Path Path.Abs Path.File)
  Read :: !(Path.Path Path.Abs Path.File) -> File Text

absolute ::
  (Member File e) =>
  Env.PurtyFilePath ->
  Eff e (Path.Path Path.Abs Path.File)
absolute = send . Absolute

read :: (Member File e) => Path.Path Path.Abs Path.File -> Eff e Text
read = send . Read

io :: File a -> IO a
io = \case
  Absolute filePath -> case filePath of
    Env.AbsFile x  -> pure x
    Env.RelFile x  -> makeAbsolute x
    Env.Unparsed x -> resolveFile' (unpack x)
  Read absFile -> readFileUtf8 (Path.fromAbsFile absFile)
