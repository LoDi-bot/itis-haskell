{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyLib (someFunc) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import System.Environment
import System.FilePath
import System.IO

data Config = Config
  { depth :: Int
    , humanReadable :: Bool
    , verbose :: Bool
    , rootDir :: FilePath
  } deriving Show

newtype AppState = AppState {currentDepth :: Int} deriving (Eq, Show)

data AppError = FileNotFoundError FilePath | OtherError String deriving (Show)

newtype App a = App
  { duApp :: ReaderT Config (StateT AppState (ExceptT AppError (WriterT [String] IO))) a }
  deriving ( Functor,
             Applicative,
             Monad,
             MonadIO,
             MonadReader Config,
             MonadState AppState,
             MonadError AppError,
             MonadWriter [String]
           )

-- Основная функция
someFunc :: IO ()
someFunc = do
  args <- liftIO getArgs
  print args
  let config = parseArgs args
  let initialState = AppState { currentDepth = 0 }
  (result, logs) <- runWriterT $ runExceptT $ runStateT (runReaderT (duApp (du (rootDir config))) config) initialState
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (totalSize, _) -> do
      putStrLn $ "Total Size: " ++ formatSize (humanReadable config) totalSize
      putStrLn "Logs:"
      mapM_ putStrLn logs

parseArgs :: [String] -> (Config)
parseArgs args =
  let config = Config
        { depth = if "-s" `elem` args then 0 else maybe (-1) read (getOption "-d" args)
        , humanReadable = "-h" `elem` args
        , verbose = "-v" `elem` args
        , rootDir = last args
        }
  in config
  where
    getOption :: String -> [String] -> Maybe String
    getOption option (opt : value : rest)
      | opt == option = Just value
      | otherwise = getOption option rest
    getOption _ _ = Nothing

-- Функция для обхода директории и подсчета размера
du :: FilePath -> App Integer
du dir = do
  config <- ask
  appState <- get
  let currentDepth' = currentDepth appState
  if currentDepth' > depth config
    then return 0
    else do
      files <- liftIO $ listDirectory dir
      sizes <- forM files $ \file -> do
        let path = dir </> file
        isDir <- liftIO $ doesDirectoryExist path
        if isDir
          then do
            modify (\s -> s { currentDepth = currentDepth' + 1 })
            du path
          else getFileSizeFromPath path
      let totalSize = sum sizes
--      when (currentDepth' == 0)
--        $ tell [dir ++ ": " ++ formatSize (humanReadable config) totalSize]
      when (verbose config)
        $ tell ["Depth " ++ show currentDepth' ++ ": " ++ dir ++ ": " ++ formatSize (humanReadable config) totalSize]
      return totalSize

-- Функция для получения размера файла
getFileSizeFromPath :: FilePath -> App Integer
getFileSizeFromPath path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then liftIO $ hFileSize =<< openFile path ReadMode
    else throwError $ FileNotFoundError path

formatSize :: Bool -> Integer -> String
formatSize True size
  | size < 1024 = show size ++ "B"
  | size < 1024^2 = show (size `div` 1024) ++ "KB"
  | size < 1024^3 = show (size `div` (1024^2)) ++ "MB"
  | size < 1024^4 = show (size `div` (1024^3)) ++ "GB"
  | size < 1024^5 = show (size `div` (1024^4)) ++ "TB"
  | otherwise = show (size `div` (1024^5)) ++ "PB"
formatSize False size = show size
