-- TODO: push upstream
module Development.Scripts.Markdown
    ( module Types
    , readMarkdown
    ) where

import Cheapskate (markdown)
import Data.Default.Class (def)
import Cheapskate.Types as Types
import qualified Data.Text.IO as T


readMarkdown :: FilePath -> IO Blocks
readMarkdown file = (blocks . markdown def) <$> T.readFile file

blocks :: Doc -> Blocks
blocks (Doc _ xs) = xs
