-- TODO: push upstream
module Development.Scripts.Markdown
    ( module Types
    , readMarkdown
    , inlinesText
    , inlineText
    ) where

import Cheapskate (markdown)
import Data.Default.Class (def)
import Cheapskate.Types as Types
import Data.Text (Text, singleton)
import qualified Data.Text.IO as T


readMarkdown :: FilePath -> IO Blocks
readMarkdown file = (blocks . markdown def) <$> T.readFile file

blocks :: Doc -> Blocks
blocks (Doc _ xs) = xs


{-# INLINE inlinesText #-}
inlinesText :: Inlines -> Text
inlinesText = foldMap inlineText

{-# INLINABLE inlineText #-}
inlineText :: Inline -> Text
inlineText = \case
    Str x        -> x
    Space        -> singleton ' '
    SoftBreak    -> mempty
    LineBreak    -> mempty
    Emph xs      -> inlinesText xs
    Strong xs    -> inlinesText xs
    Code x       -> x
    Link xs _ _  -> inlinesText xs
    Image xs _ _ -> inlinesText xs
    Entity x     -> x
    RawHtml x    -> x
