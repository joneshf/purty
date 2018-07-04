module Output where

import "rio" RIO hiding (withSystemTempFile)

import "freer-simple" Control.Monad.Freer                    (Eff, Member, send)
import "prettyprinter" Data.Text.Prettyprint.Doc             (SimpleDocStream)
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "path" Path                                           (Abs, File, Path)
import "path-io" Path.IO
    ( copyFile
    , copyPermissions
    , withSystemTempFile
    )

data Output a where
  InPlace :: !(Path Abs File) -> !(SimpleDocStream a) -> Output ()
  StdOut :: !(SimpleDocStream a) -> Output ()

inPlace :: (Member Output e) => Path Abs File -> SimpleDocStream a -> Eff e ()
inPlace absFile = send . InPlace absFile

stdOut :: (Member Output e) => SimpleDocStream a -> Eff e ()
stdOut = send . StdOut

io :: Output a -> IO a
io = \case
  InPlace absFile stream -> withSystemTempFile "purty.purs" $ \tempFile h -> do
    renderIO h stream
    hClose h
    copyPermissions absFile tempFile
    copyFile tempFile absFile
  StdOut stream -> renderIO stdout stream
