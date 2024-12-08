module CommandLine (parseCommand) where
import GHC.Base (undefined)



-- import Options.Applicative

data Command = Command FilePath Idendity Idendity
type Idendity = String

parseCommand :: IO Command
parseCommand = undefined
-- parseCommand = execParser parser

-- parser :: ParserInfo Command
-- parser = info (sample <**> helper)
--     ( fullDesc
--     <> progDesc "Compute trust relations between two idendities."
--     <> header "HTPL - a High-level Trust Policy Language for P3KI Core"
    -- )