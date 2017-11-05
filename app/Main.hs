import Data.Maybe
import Data.Semigroup ((<>))
import System.IO

import CMark
import Data.Text hiding (empty, foldl)
import Data.Text.IO as TIO
import Options.Applicative

import Text.CommonMark.Sub

data HeadingPattern = HeadingPattern Level Text deriving (Eq, Ord, Show)

data Extraction = Extraction 
    { outputFilePath :: FilePath
    , headingPattern :: HeadingPattern
    , ignoreCase :: Bool
    , omitHeading :: Bool
    , columnWidth :: Maybe Int
    , inputFilePath :: FilePath
    } deriving (Eq, Ord, Show)

headingLevel :: Parser HeadingPattern
headingLevel =
    foldl (<|>) empty parsers
  where
    levels :: [Level]
    levels = [1..6]
    parsers :: [Parser HeadingPattern]
    parsers =
        [ (HeadingPattern l . pack) <$> strOption
              (  long ("h" ++ show l)
              <> metavar "TITLE"
              <> help ("Extract the section with the heading level " ++
                       show l ++ ".  Mutually exclusive with other --h* " ++
                       "options")
              )
        | l <- levels
        ]

parser :: Parser Extraction
parser = Extraction
    <$> strOption (  long "out-file"
                  <> short 'o'
                  <> metavar "FILE"
                  <> value "-"
                  <> showDefault
                  <> help "Write output to FILE"
                  )
    <*> headingLevel
    <*> switch (  long "ignore-case"
               <> short 'i'
               <> help "Ignore case distinctions"
               )
    <*> switch (  long "omit-heading"
               <> short 'O'
               <> help "Omit a leading heading"
               )
    <*> ( Just . read <$> strOption
            (  long "columns"
            <> short 'c'
            <> metavar "WIDTH"
            <> help ("Limit the maximum characters per line of the output.  " ++
                     "No limit by default")
            )
        <|> pure Nothing
        )
    <**> helper
    <*> strArgument (  metavar "FILE"
                    <> help "CommonMark/Markdown text to extract from"
                    )

parserInfo :: ParserInfo Extraction
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
extract e@Extraction { headingPattern = (HeadingPattern level title)
                     , ignoreCase = ignoreCase'
                     , omitHeading = omitHeading'
                     , columnWidth = width
                     } = do
    text <- withInputFile e TIO.hGetContents
    let doc = commonmarkToNode [] text
        node = case (omitHeading', extractSection level equals title doc) of
            (True, Node p DOCUMENT (_ : xs)) -> Node p DOCUMENT xs
            (_, other) -> other
        result = nodeToCommonmark [] (Just $ fromMaybe (-1) width) node
    withOutputFile e (`TIO.hPutStrLn` result)
  where
    equals :: Text -> Text -> Bool
    equals = if ignoreCase' then (\ a b -> toLower a == toLower b) else (==)

main :: IO ()
main = execParser parserInfo >>= extract
