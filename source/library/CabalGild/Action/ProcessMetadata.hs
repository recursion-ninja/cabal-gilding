{-# LANGUAGE OverloadedStrings #-}

module CabalGild.Action.ProcessMetadata (
  run,
  reorderMetadataFields,
  metadataFieldNameSet,
  metadataFieldNameSetInline,
) where

import Data.Ord (comparing)
import qualified CabalGild.Type.Comment as Comment
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.Fields as Fields
import qualified Distribution.Fields.Field as Fields
import qualified Distribution.Parsec.Position as Position


-- | A wrapper around 'reorderMetadataFields' to allow this to be composed with other actions.
run ::
  (Applicative m) =>
  CabalSpecVersion.CabalSpecVersion ->
  ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], cs) ->
  m ([Fields.Field (Position.Position, [Comment.Comment Position.Position])], cs)
run csv (fs, cs) = pure (reorderMetadataFields csv fs, cs)


-- | Reorder the fields so that metadata comes first. 
reorderMetadataFields ::
  CabalSpecVersion.CabalSpecVersion ->
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])]
reorderMetadataFields = const reorderFields


reorderFields ::
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])] ->
  [Fields.Field (Position.Position, [Comment.Comment Position.Position])]
reorderFields fs =
    let comparator
          :: Fields.Field (Position.Position, [Comment.Comment Position.Position])
          -> Fields.Field (Position.Position, [Comment.Comment Position.Position])
          -> Ordering
        comparator = comparing locator

        locator
          :: Fields.Field (Position.Position, [Comment.Comment Position.Position])
          -> Maybe Int
        locator = metadataIndex . Fields.getName . Fields.fieldName

        metadataIndex :: Fields.FieldName -> Maybe Int
        metadataIndex = flip List.elemIndex metadataFieldNameOrdering

        selector :: Fields.Field ann -> Bool
        selector f =
            let fName = Fields.getName $ Fields.fieldName f
            in  fName `Set.member` metadataFieldNameSet

        (metadata, others) = List.partition selector fs
    in  List.sortBy comparator metadata <> others


metadataFieldNameOrdering :: [ Fields.FieldName ]
metadataFieldNameOrdering =
    metadataInline <> metadataBlocks


metadataInline :: [ Fields.FieldName ]
metadataInline =
    [ "cabal-version"
    , "name"
    , "version"
    , "stability"
    , "build-type"
    , "author"
    , "copyright"
    , "license"
    , "license-file"
    , "maintainer"
    , "homepage"
    , "bug-reports"
    , "synopsis"
    ]

  
metadataBlocks :: [ Fields.FieldName ]
metadataBlocks =
    [ "description"
    , "category"
    , "tested-with"
    , "data-files"
    , "extra-doc-files"
    , "extra-source-files"
    , "source-repository"
    , "flag"
    ]


metadataFieldNameSetInline :: Set.Set Fields.FieldName
metadataFieldNameSetInline = Set.fromList metadataInline


metadataFieldNameSet :: Set.Set Fields.FieldName
metadataFieldNameSet = Set.fromList metadataFieldNameOrdering
