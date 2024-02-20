{-# LANGUAGE OverloadedStrings #-}

module CabalGild.Action.Render where

import Data.Foldable
import Data.Ord (comparing)
import qualified CabalGild.Action.ProcessMetadata as Meta
import qualified CabalGild.Extra.FieldLine as FieldLine
import qualified CabalGild.Extra.Name as Name
import qualified CabalGild.Extra.SectionArg as SectionArg
import qualified CabalGild.Type.Block as Block
import qualified CabalGild.Type.Chunk as Chunk
import qualified CabalGild.Type.Comment as Comment
import qualified CabalGild.Type.Line as Line
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.Fields as Fields
import qualified Distribution.Fields.Field as Fields

data RenderingMode = Dynamic | StaticInlining
  deriving (Eq)

-- | A wrapper around 'toByteString' to allow this to be composed with other
-- actions.
run ::
  (Applicative m) =>
  ([Fields.Field [Comment.Comment a]], [Comment.Comment a]) ->
  m ByteString.ByteString
run = pure . uncurry toByteString

-- | Renders the given fields and comments to a byte string.
toByteString ::
  [Fields.Field [Comment.Comment a]] ->
  [Comment.Comment a] ->
  ByteString.ByteString
toByteString fs cs =
  let i = 0 :: Int
   in Block.toByteString
        . Lens.set Block.lineBeforeLens False
        . Lens.set Block.lineAfterLens True
        $ fields Dynamic i fs <> comments i cs


-- | Renders the given fields to a block at the given indentation level.
fields :: RenderingMode -> Int -> [Fields.Field [Comment.Comment a]] -> Block.Block
fields mode = foldMap . field mode

-- | Renders the given field to a block at the given indentation level.
--
-- If a field only has one line and no comments, then it can be rendered all on
-- one line.
field :: RenderingMode -> Int -> Fields.Field [Comment.Comment a] -> Block.Block
field mode i f = case f of
  Fields.Field n fls -> case fls of
    [fl]
      | null (FieldLine.annotation fl) && Fields.getName n `Set.member` Meta.metadataFieldNameSet -> renderFieldInline i n fl
    _ | mode == StaticInlining -> foldMap (renderFieldInline i n) fls 
    _ ->
      Lens.set Block.lineAfterLens True $
        comments i (Name.annotation n)
          <> Block.fromLine
            Line.Line
              { Line.indent = i,
                Line.chunk = name n <> Chunk.colon
              }
          <> fieldLines (i + 1) fls
  Fields.Section n _ _ | mode == Dynamic && Fields.getName n `Set.member` Meta.metadataFieldNameSetSpecial -> field StaticInlining i f
  Fields.Section n sas fs ->
    let blockSpacing = case mode of
          Dynamic -> Block.lineBeforeLens
          StaticInlining -> Block.lineAfterLens
    in  Lens.set Block.lineBeforeLens True
          . Lens.set Block.lineAfterLens True
          $ comments i (Name.annotation n)
            <> comments i (concatMap SectionArg.annotation sas)
            <> Block.fromLine
              Line.Line
                { Line.indent = i,
                  Line.chunk = Lens.set Chunk.spaceAfterLens True (name n) <> sectionArgs sas
                }
            <> Lens.set blockSpacing False (fields mode (i + 1) fs)

renderFieldInline :: Int -> Fields.Name [Comment.Comment a] -> Fields.FieldLine a2 -> Block.Block
renderFieldInline i n fl =
    let fComments = comments i (Name.annotation n)
        fContent = Block.fromLine
                   . Lens.over Line.chunkLens (mappend $ metadataFieldPadding n)
                   $ fieldLine i fl
    in  fComments <> fContent


-- | Renders the given name to a chunk.
name :: Fields.Name a -> Chunk.Chunk
name = Chunk.fromByteString . Name.value

-- | Renders the given field lines to a block at the given indentation level.
fieldLines :: Int -> [Fields.FieldLine [Comment.Comment a]] -> Block.Block
fieldLines = foldMap . fieldLineC

-- | Renders the given field line and its comments to a block at the given
-- indentation level.
fieldLineC :: Int -> Fields.FieldLine [Comment.Comment a] -> Block.Block
fieldLineC i fl =
  comments i (FieldLine.annotation fl)
    <> Block.fromLine (fieldLine i fl)

-- | Renders the given field line to a line at the given indentation level.
fieldLine :: Int -> Fields.FieldLine a -> Line.Line
fieldLine i =
  Line.Line i
    . Lens.set Chunk.spaceBeforeLens True
    . Chunk.fromByteString
    . FieldLine.value

-- | Renders the given section arguments to a chunk. Note that comments are
-- ignored. In practice this isn't a problem because section arguments can't
-- have comments attached anyway.
sectionArgs :: [Fields.SectionArg a] -> Chunk.Chunk
sectionArgs = Lens.set Chunk.spaceBeforeLens True . foldMap sectionArg

-- | Renders the given section argument to a chunk.
sectionArg :: Fields.SectionArg a -> Chunk.Chunk
sectionArg sa = case sa of
  Fields.SecArgName _ bs ->
    Lens.set Chunk.spaceBeforeLens True
      . Lens.set Chunk.spaceAfterLens True
      $ Chunk.fromByteString bs
  Fields.SecArgStr _ bs ->
    Lens.set Chunk.spaceBeforeLens True
      . Lens.set Chunk.spaceAfterLens True
      . Chunk.fromByteString
      . flip ByteString.snoc 0x22
      $ ByteString.cons 0x22 bs
  Fields.SecArgOther _ bs ->
    let b =
          bs /= ByteString.singleton 0x21 -- !
            && bs /= ByteString.singleton 0x28 -- (
            && bs /= ByteString.singleton 0x29 -- )
     in Lens.set Chunk.spaceBeforeLens b
          . Lens.set Chunk.spaceAfterLens b
          $ Chunk.fromByteString bs

-- | Renders the given comments to a block at the given indentation level.
comments :: Int -> [Comment.Comment a] -> Block.Block
comments i cs = mempty {Block.lines = fmap (comment i) cs}

-- | Renders the given comment to a line at the given indentation level.
comment :: Int -> Comment.Comment a -> Line.Line
comment i =
  Line.Line i
    . Chunk.fromByteString
    . mappend Comment.delimiter
    . Comment.value


metadataFieldPadding :: Fields.Name a -> Chunk.Chunk
metadataFieldPadding fName =
    let longest = maximumBy (comparing ByteString.length) Meta.metadataFieldNameSet
        fullLen = ByteString.length longest
        nameLen = ByteString.length $ Fields.getName fName
        moreLen = case fullLen - nameLen of
            0 -> 0
            n -> n + 1
        padding = BS8.replicate moreLen ' ' 
    in  name fName <> Chunk.colon <> Chunk.fromByteString padding
