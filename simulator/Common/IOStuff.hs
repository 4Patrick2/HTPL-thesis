{-# LANGUAGE LambdaCase #-}

module Common.IOStuff
    ( handleInput
    , exitWithError
    , toTextType
    , TextType
    , Input(..)
    ) where

import           Control.Exception  ( catch, IOException )
import           System.IO.Error    ( isDoesNotExistError )
import           System.Exit        ( exitWith, ExitCode(..) )
import qualified Data.Text          as T

------------------------
-- | Handling file input
------------------------

type TextType = T.Text

toTextType :: String -> TextType
toTextType = T.pack

type InputString  = String
type PathString   = String

data Input = FileInput PathString
           | StdInput  InputString
    deriving (Eq, Show)

handleInput :: Input -> IO (TextType, FilePath)
handleInput input = case input of
    StdInput s -> return (T.pack s, "stdin")
    FileInput f -> readFileSafely f >>= \case
        Left err -> exitWithError err
        Right s -> return (T.pack s, f)
  where
    readFileSafely :: FilePath -> IO (Either String String)
    readFileSafely filePath = catch (Right <$> readFile filePath) handler
      where
        handler :: IOException -> IO (Either String String)
        handler e
            | isDoesNotExistError e = return . Left $ "File not found: " ++ filePath
            | otherwise = return . Left $ "Error reading file: " ++ show e


----------------
-- | Error stuff
----------------

exitWithError :: String -> IO a
exitWithError errorMsg = do
    putStrLn errorMsg
    exitError

exitError :: IO a
exitError = exitWith (ExitFailure 1)