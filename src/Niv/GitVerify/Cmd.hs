{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.GitVerify.Cmd where

import Control.Applicative
import Control.Arrow
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Data.String.QQ (s)
import qualified Data.Text as T
import Data.Text.Extended as T
import Niv.Cmd
import Niv.Git.Cmd
import Niv.Sources
import Niv.Update
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

gitVerifyCmd :: Cmd
gitVerifyCmd =
  Cmd
    { description = describeGitVerify,
      parseCmdShortcut = parseGitVerifyShortcut,
      parsePackageSpec = parseGitVerifyPackageSpec,
      updateCmd = gitVerifyUpdate',
      name = "gitverify",
      extraLogs = const []
    }

parseGitVerifyShortcut :: T.Text -> Maybe (PackageName, Aeson.Object)
parseGitVerifyShortcut txt'@(T.dropWhileEnd (== '/') -> txt) =
  -- basic heuristics for figuring out if something is a git repo
  if isGitURL
    then case T.splitOn "/" txt of
      [] -> Nothing
      (last -> w) -> case T.stripSuffix ".git" w of
        Nothing -> Just (PackageName w, KM.singleton "url" (Aeson.String txt'))
        Just w' -> Just (PackageName w', KM.singleton "url" (Aeson.String txt'))
    else Nothing
  where
    isGitURL =
      ".git"
        `T.isSuffixOf` txt
        || "git@"
          `T.isPrefixOf` txt
        || "ssh://"
          `T.isPrefixOf` txt

parseGitVerifyPackageSpec :: Opts.Parser PackageSpec
parseGitVerifyPackageSpec =
  PackageSpec . KM.fromList
    <$> many (parseUrl <|> parseBranch <|> parseRev <|> parseAttr <|> parseSAttr <|> parseIntro)
  where
    parseUrl =
      ("url",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "url"
              <> Opts.metavar "URL"
          )
    parseRev =
      ("rev",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "rev"
              <> Opts.metavar "SHA"
          )
    parseBranch =
      ("branch",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "branch"
              <> Opts.short 'b'
              <> Opts.metavar "BRANCH"
          )
    parseIntro =
      ("intro",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "intro"
              <> Opts.short 'i'
              <> Opts.metavar "SHA"
          )
    parseAttr =
      Opts.option
        (Opts.maybeReader parseKeyValJSON)
        ( Opts.long "attribute"
            <> Opts.short 'a'
            <> Opts.metavar "KEY=VAL"
            <> Opts.help "Set the package spec attribute <KEY> to <VAL>, where <VAL> may be JSON."
        )
    parseSAttr =
      Opts.option
        (Opts.maybeReader (parseKeyVal Aeson.toJSON))
        ( Opts.long "string-attribute"
            <> Opts.short 's'
            <> Opts.metavar "KEY=VAL"
            <> Opts.help "Set the package spec attribute <KEY> to <VAL>."
        )
    parseKeyValJSON = parseKeyVal $ \x ->
      fromMaybe (Aeson.toJSON x) (Aeson.decodeStrict (B8.pack x))
    -- Parse "key=val" into ("key", val)
    parseKeyVal ::
      -- how to convert to JSON
      (String -> Aeson.Value) ->
      String ->
      Maybe (K.Key, Aeson.Value)
    parseKeyVal toJSON str = case span (/= '=') str of
      (key, '=' : val) -> Just (K.fromString key, toJSON val)
      _ -> Nothing

describeGitVerify :: Opts.InfoMod a
describeGitVerify =
  mconcat
    [ Opts.fullDesc,
      Opts.progDesc "Add a git-verify dependency. Experimental.",
      Opts.headerDoc $
        Just $
          Opts.vcat
            [ "Examples:",
              "",
              "TODO"
            ]
    ]

gitVerifyUpdate ::
  -- | prefetch
  (T.Text -> T.Text -> IO T.Text) ->
  -- | latest rev
  (T.Text -> T.Text -> IO T.Text) ->
  -- | latest rev and default ref
  (T.Text -> IO (T.Text, T.Text)) ->
  Update () ()
gitVerifyUpdate prefetch latestRev' defaultBranchAndRev' = proc () -> do
  useOrSet "type" -< ("gitverify" :: Box T.Text)
  url <- load "url" -< ()
  discoverRefAndRev -< url
  rev <- discoverRev -< url
  update "hash" <<< run' (uncurry prefetch) -< (,) <$> url <*> rev
  returnA -< ()
  where
    discoverRefAndRev = proc url -> do
      branchAndRev <- run defaultBranchAndRev' -< url
      update "branch" -< fst <$> branchAndRev
      update "rev" -< snd <$> branchAndRev
      returnA -< ()
    discoverRev = proc url -> do
      branch <- load "branch" -< ()
      rev <- run' (uncurry latestRev') -< (,) <$> url <*> branch
      update "rev" -< rev
      returnA -< rev

-- | The "real" (IO) update
gitVerifyUpdate' :: Update () ()
gitVerifyUpdate' = gitVerifyUpdate nixPrefetchGit latestRev defaultBranchAndRev

nixPrefetchGit :: T.Text -> T.Text -> IO T.Text
nixPrefetchGit (T.unpack -> url) (T.unpack -> rev) = do
  (exitCode, sout, serr) <- runNixPrefetch
  case exitCode of
    ExitSuccess -> case getHash sout of
      Nothing -> abort "Could not decode output of 'nix-prefetch-git'."
      Just hash -> pure hash
    _ -> abortNixPrefetchGitExpectedOutput (T.pack <$> args) (T.pack sout) (T.pack serr)
  where
    args = ["--deepClone", "--leave-dotGit", url, rev]
    runNixPrefetch = readProcessWithExitCode "nix-prefetch-git" args ""
    getHash :: String -> Maybe T.Text
    getHash input = do
      obj :: Aeson.Object <- Aeson.decodeStrict $ B8.pack input
      case KM.lookup "hash" obj of
        Just (Aeson.String hash) -> pure hash
        _ -> Nothing

abortNixPrefetchGitExpectedOutput :: [T.Text] -> T.Text -> T.Text -> IO a
abortNixPrefetchGitExpectedOutput args sout serr =
  abort $
    [s|
Could not read the output of 'nix-prefetch-git. This is a bug. Please create a
ticket:

  https://github.com/nmattia/niv/issues/new

Thanks! I'll buy you a beer.
|]
      <> T.unlines ["command: ", "nix-prefetch-git" <> T.unwords args, "stdout: ", sout, "stderr: ", serr]
