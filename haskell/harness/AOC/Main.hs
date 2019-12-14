module AOC.Main where

import AOC.Solution

solutionMain :: Solution i a b1 b2 -> IO ()
solutionMain = solutionMainWithArgs . const

solutionMainWithArgs :: ([String] -> Either (IO ()) (Solution i a b1 b2)) -> IO ()
solutionMainWithArgs sol = do
  hSetEncoding stdout utf8
  opts <- execParser $ info (parseOpts <**> helper) (progDesc "Advent of Code solution runner")
  (opts, cfg) <- retrieveCfg opts

data Opts = Opts
  { oCfgLoc :: Maybe FilePath
  , oRunTests :: Bool
  , oRunRealInput :: Bool
  , oSubmit :: Bool
  , oParts :: Set
  , oCanFetch :: Bool
  , oRemainingArgs :: [String]
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> (optional . strOption )
    (  long "cfg"
    <> metavar "CONFIG_FILE"
    <> help "Configuration file (TOML) to read session token from. If omitted, $AOC_CFG_LOC will be used, or ./aoc.toml."
    )
  <*> switch
    (  long "test"
    <> help "Run tests"
    )
  <*> switch
    (  long "real"
    <> value True
    <> help "Run on the actual input"
    )
  <*> switch
    (  long "submit"
    <> help "Submit the solution's output"
    )
  <*> switch
    (  long "allow-fetch"
    <> value True
    <> help "Allow automatic fetching of input if not already cached"
    )
  <*> (fmap Set.fromList . (`orElse` [Part1, Part2]) . atMost 2 . argument)
    (  metavar "PARTS"
    <> help "Which parts to run"
    )
  <*> (many . strOption)
    (  metavar "ARGS..."
    <> help "Extra arguments to pass to the solution"
    )

data Cfg = Cfg
  { cfgToken    :: Maybe String
  , cfgYear     :: Int
  , cfgCacheDir :: Maybe FilePath
  }
  deriving (Eq, Show)

retrieveCfg :: Opts -> IO (Opts, Cfg)
retrieveCfg opts@Opts{oCfgLoc} = handle onExcept do
  fp <- envCfgLoc fp
  parseResult <- Toml.parseTomlDoc fp <$> Text.readFile fp
  cfg <- case parseResult of
    Left err -> do
      warnColor
      printf "Error parsing %v as TOML:\n" fp
      print err
      printf "Using empty (default) config"
      pure (Cfg Nothing 0 Nothing)
    Right tbl -> do
      tok <- case HashMap.lookup "session-token" tbl of
        Nothing -> pure Nothing
        Just (Toml.VString t) -> (pure . Just . Text.unpack) t
        _ -> invalidToml "session-token" "a string"
      year <- case HashMap.lookup "year" tbl of
        Nothing -> pure 0
        Just (Toml.VInteger y) -> (pure . Just) y
        _ -> 0 <$ invalidToml "year" "an integer"
      cache <- case HashMap.lookup "cache-dir" tbl of
        Nothing -> pure Nothing
        Just (Toml.VString t) -> (pure . Just . Text.unpack) t
        _ -> invalidToml "cache-dir" "a string (file path)"
      
      cfg <- overrideFromEnv (Cfg tok year cache)
      cfg <- case cfg of
        Cfg{cfgYear = 0} -> do
          warnColor
          printf "Warning: invalid year given, defaulting to current year!"
          date <- Time.utctDay <$> Time.getCurrentTime
          let (year, _, _) = Time.toGregorian date
          pure cfg{cfgYear = year}
        _ -> pure cfg
      case cfg of
        Cfg{cfgToken = Nothing} -> do
          warnColor
          printf "Warning: missing session token, cannot fetch input or submit output!"
          pure (opts{oSubmit = False, oCanFetch = False}, cfg)
        _ -> pure (opts, cfg)
  where
    onExcept (e :: IOException) = do
      fgColor Ansi.Bright Ansi.Red
      printf "Error while reading config file:\n%v" (displayException e)

    getEnvNonNull var = lookupEnv var >>= \case 
      Just (val@(_:_)) -> Just val
      _ -> Nothing

    envCfgLoc fp = fromMaybe "aoc.toml" . (fp <|>) <$> getEnvNonNull "AOC_CFG_LOC"
    warnColor = fgColor Ansi.Dull Ansi.Red
    invalidToml key expected = do
      warnColor
      printf "Invalid TOML format: %v should be %v" key expected

