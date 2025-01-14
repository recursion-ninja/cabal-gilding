module CabalGild.Action.GetCabalVersion where

import qualified CabalGild.Extra.Either as Either
import qualified CabalGild.Extra.FieldLine as FieldLine
import qualified CabalGild.Extra.Name as Name
import qualified CabalGild.Extra.String as String
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Distribution.CabalSpecVersion as CabalSpecVersion
import qualified Distribution.FieldGrammar.Newtypes as Newtypes
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec

-- | Gets the Cabal spec version from a list of fields. If multiple fields
-- define the version, the first one is used.
fromFields :: [Fields.Field a] -> CabalSpecVersion.CabalSpecVersion
fromFields fs = Maybe.fromMaybe CabalSpecVersion.CabalSpecV1_0 $ do
  f <- List.find isCabalVersion fs
  fls <- getFieldLines f
  Newtypes.getSpecVersion <$> fromFieldLines fls

-- | Returns true if the field is named @cabal-version@.
isCabalVersion :: Fields.Field a -> Bool
isCabalVersion f = case f of
  Fields.Field n _ -> Name.value n == String.toUtf8 "cabal-version"
  Fields.Section {} -> False

-- | Gets the field lines from a field. If the field is a section, this returns
-- 'Nothing'.
getFieldLines :: Fields.Field a -> Maybe [Fields.FieldLine a]
getFieldLines f = case f of
  Fields.Field _ fls -> Just fls
  Fields.Section {} -> Nothing

-- | Attempts to parse the given field lines as a Cabal spec version.
fromFieldLines :: [Fields.FieldLine a] -> Maybe Newtypes.SpecVersion
fromFieldLines =
  Either.hush
    . Parsec.runParsecParser Parsec.parsec ""
    . FieldLine.toFieldLineStream
