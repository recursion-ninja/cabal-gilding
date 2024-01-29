-- |
-- License: BSD-3-Clause
-- Copyright: Oleg Grenrus
module CabalGild.Main (main) where

import CabalGild (cabalGild)
import CabalGild.Error (Error (SomeError), renderError)
import CabalGild.Monad (runCabalGildIO)
import CabalGild.Options
import CabalGild.Prelude
import Control.Applicative (many, (<**>))
import qualified Data.ByteString as BS
import Data.Traversable (for)
import Data.Version (showVersion)
import qualified Options.Applicative as O
import Paths_cabal_gild (version)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  (opts', filepaths) <- O.execParser optsP'
  let opts = runOptionsMorphism opts' defaultOptions

  notFormatted <-
    catMaybes <$> case filepaths of
      [] -> fmap pure $ BS.getContents >>= main' opts Nothing
      (_ : _) -> for filepaths $ \filepath -> do
        contents <- BS.readFile filepath
        main' opts (Just filepath) contents

  when ((optMode opts == ModeCheck) && not (null notFormatted)) $ do
    for_ notFormatted $ \filepath ->
      hPutStrLn stderr $ "error: Input " <> filepath <> " is not formatted."
    exitFailure
  where
    optsP' =
      O.info (optsP <**> O.helper <**> versionP) $
        mconcat
          [ O.fullDesc,
            O.progDesc "Reformat .cabal files",
            O.header "cabal-gild - .cabal file reformatter"
          ]

    versionP :: O.Parser (a -> a)
    versionP =
      O.infoOption (showVersion version) $
        O.long "version" <> O.help "Show version"

main' :: Options -> Maybe FilePath -> BS.ByteString -> IO (Maybe FilePath)
main' opts mfilepath input = do
  -- name of the input
  let filepath = fromMaybe "<stdin>" mfilepath

  mroot <-
    fmap takeDirectory <$> case (mfilepath, optStdinInputFile opts) of
      (Just _, Just _) -> do
        renderError $ SomeError "cannot pass both --stdin-input-file and FILE"
        exitFailure
      (Just f, Nothing) -> pure $ Just f
      (Nothing, Just f) -> pure $ Just f
      (Nothing, Nothing) -> pure Nothing

  -- process
  res <- runCabalGildIO mroot opts (cabalGild filepath input)

  case res of
    Right output -> do
      let outputBS = toUTF8BS output
          formatted = outputBS == input

      case optMode opts of
        ModeStdout -> BS.putStr outputBS
        ModeInplace -> case mfilepath of
          Nothing -> BS.putStr outputBS
          Just _ -> unless formatted $ BS.writeFile filepath outputBS
        _ -> return ()

      return $ if formatted then Nothing else Just filepath
    Left err -> do
      renderError err
      exitFailure

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

optsP :: O.Parser (OptionsMorphism, [FilePath])
optsP =
  (,)
    <$> optsP'
    <*> many (O.strArgument (O.metavar "FILE..." <> O.help "input files"))
  where
    optsP' =
      fmap mconcat $
        many $
          asum
            [ werrorP,
              noWerrorP,
              indentP,
              tabularP,
              noTabularP,
              cabalFileP,
              noCabalFileP,
              stdoutP,
              inplaceP,
              checkP,
              rootP
            ]

    werrorP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optError = True}) $
        O.long "Werror" <> O.help "Treat warnings as errors"

    noWerrorP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optError = False}) $
        O.long "Wno-error"

    indentP =
      O.option (fmap (\n -> mkOptionsMorphism $ \opts -> opts {optIndent = n}) O.auto) $
        O.long "indent" <> O.help "Indentation" <> O.metavar "N"

    tabularP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optTabular = True}) $
        O.long "tabular" <> O.help "Tabular formatting"

    noTabularP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optTabular = False}) $
        O.long "no-tabular"

    cabalFileP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optCabalFile = True}) $
        O.long "cabal-file"

    noCabalFileP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optCabalFile = False}) $
        O.short 'n' <> O.long "no-cabal-file" <> O.help "Don't parse as .cabal file"

    stdoutP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optMode = ModeStdout}) $
        O.long "stdout" <> O.help "Write output to stdout (default)"

    inplaceP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optMode = ModeInplace}) $
        O.short 'i' <> O.long "inplace" <> O.help "Process files in-place"

    checkP =
      O.flag' (mkOptionsMorphism $ \opts -> opts {optMode = ModeCheck}) $
        O.short 'c' <> O.long "check" <> O.help "Fail with non-zero exit code if input is not formatted"

    rootP =
      O.option (fmap (\f -> mkOptionsMorphism $ \opts -> opts {optStdinInputFile = Just f}) O.str) $
        O.long "stdin-input-file" <> O.help "When reading from STDIN, use this file path to resolve relative references" <> O.metavar "FILE"