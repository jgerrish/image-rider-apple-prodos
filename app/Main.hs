module Main where

import qualified ImageRider (someFunc)
import qualified System.Environment as Env (getArgs)

-- From Effective Haskell
-- TODO Explain how it coerces String to FilePath
-- Either here is a common Haskell error type, it has a Left and Right side
-- The Left side is usually the error.
handleArgs :: IO [String] -> IO (Either String FilePath)
handleArgs args =
  -- <$> is the infix version of fmap
  -- The type we're passed in isn't a String list ([String]), it's an
  -- IO [String], which is why we need a map (fmap) that works over
  -- other structures.
  -- TODO: Better error handling
  parseArgs <$> args
  where
    parseArgs parsedArgs =
      case parsedArgs of
        (arg : _rest) -> case _rest of
          [] -> Right arg
          _ -> Left "There should only be one argument, the image filename"
        [] -> Left "The first argument should be a path to an image file"

main :: IO ()
main = do
  -- args <- Env.getArgs
  handleArgsResult <- handleArgs Env.getArgs
  -- TODO Do this in a more idiomatic way (? exception bubbling, or
  -- whatever Haskell style is)
  case handleArgsResult of
    Left errorMessage ->
      putStrLn $ "Error: " <> errorMessage
    Right filename -> ImageRider.someFunc filename
