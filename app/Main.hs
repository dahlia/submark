{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
import Data.Maybe
import Data.Version
import System.IO

import CMark
import Data.Text hiding (empty, foldl)
import Data.Text.IO as TIO
import Options.Applicative

import Text.CommonMark.Sub
import qualified Paths_submark as Meta

data Command
    = Extract Extraction
    | ShowVersion
    deriving (Eq, Ord, Show)

data Extraction = Extraction
    { outputFilePath :: FilePath
    , headingPattern :: HeadingPattern
    , ignoreCase :: Bool
    , omitHeading :: Bool
    , columnWidth :: Maybe Int
    , inputFilePath :: FilePath
    } deriving (Eq, Ord, Show)

headingPatternParser :: Parser HeadingPattern
headingPatternParser =
    foldl (<|>) empty parsers
  where
    levels :: [Level]
    levels = [1..6]
    parsers :: [Parser HeadingPattern]
    parsers = titleTextParsers ++ titleRegexParsers
    titleTextParsers :: [Parser HeadingPattern]
    titleTextParsers =
        [ HeadingPattern l False . HeadingTitleText . pack <$> strOption
              (  long ("h" ++ show l)
              <> metavar "TITLE"
              <> help ("Extract the section with the heading level " ++
                       show l ++ ".  Note that it tries to match to the " ++
                       "heading title with no markup, which means " ++
                       "--h1 \"foo bar\" matches to both `# foo bar' and " ++
                       "`# _foo_ **bar**'.  Mutually exclusive with other " ++
                       "--h* and --h*-regex options.")
              )
        | l <- levels
        ]
    titleRegexParsers :: [Parser HeadingPattern]
    titleRegexParsers =
        [ HeadingPattern l False . HeadingTitleRegex . pack <$> strOption
              (  long ("h" ++ show l ++ "-regex")
              <> metavar "PATTERN"
              <> help ("Similar to --h" ++ show l ++ " except that it takes " ++
                       "a regular expression.  Note that it tries to match " ++
                       "to the heading title with no markup, which means " ++
                       "--h1 \"fo+ ba[rz]\" matches to both `# foo bar' and " ++
                       "`# _foooo_ **baz**'.  Mutually exclusive with other " ++
                       "--h* and --h*-regex options.")
              )
        | l <- levels
        ]

extractionParser :: Parser Extraction
extractionParser = Extraction
    -- CHECK: Write docs in README.md for a new option.
    <$> strOption (  long "out-file"
                  <> short 'o'
                  <> metavar "FILE"
                  <> value "-"
                  <> showDefault
                  <> help "Write output to FILE."
                  )
    <*> headingPatternParser
    <*> switch (  long "ignore-case"
               <> short 'i'
               <> help "Ignore case distinctions."
               )
    <*> switch (  long "omit-heading"
               <> short 'O'
               <> help "Omit a leading heading."
               )
    <*> ( Just . read <$> strOption
            (  long "columns"
            <> short 'c'
            <> metavar "WIDTH"
            <> help ("Limit the maximum characters per line of the output.  " ++
                     "No limit by default.")
            )
        <|> pure Nothing
        )
    <**> helper
    <*> strArgument (  metavar "FILE"
                    <> help "CommonMark/Markdown text to extract from."
                    )

showVersionParser :: Parser Command
showVersionParser = flag' ShowVersion
    ( long "version"
    <> short 'v'
    <> help "Show version."
    )

parser :: Parser Command
parser = (Extract <$> extractionParser) <|> showVersionParser

parserInfo :: ParserInfo Command
parserInfo = info parser $
    fullDesc <> progDesc "Extract a part from CommonMark/Markdown docs."

withInputFile :: Extraction -> (Handle -> IO r) -> IO r
withInputFile Extraction { inputFilePath = "-" } action' = do
    let i = stdin
    r <- action' i
    hClose i
    return r
withInputFile Extraction { inputFilePath = i } action' =
    withFile i ReadMode action'

withOutputFile :: Extraction -> (Handle -> IO r) -> IO r
withOutputFile Extraction { outputFilePath = "-" } action' = do
    let o = stdout
    r <- action' o
    hClose o
    return r
withOutputFile Extraction { outputFilePath = o } action' =
    withFile o WriteMode action'

extract :: Extraction -> IO ()
extract e@Extraction { headingPattern
                     , ignoreCase
                     , omitHeading
                     , columnWidth
                     } = do
    text <- withInputFile e TIO.hGetContents
    let doc = commonmarkToNode [] text
        pat = headingPattern { caseInsensitive = ignoreCase }
        node = case (omitHeading, extractSection pat doc) of
            (True, Node p DOCUMENT (_ : xs)) -> Node p DOCUMENT xs
            (_, other) -> other
        result = nodeToCommonmark [] (Just $ fromMaybe (-1) columnWidth) node
    withOutputFile e (`TIO.hPutStrLn` result)

main :: IO ()
main = execParser parserInfo >>= \ case
    Extract e -> extract e
    ShowVersion -> Prelude.putStrLn $ showVersion Meta.version
