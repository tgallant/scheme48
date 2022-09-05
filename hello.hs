module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "Enter a name:"
  name <- getLine
  putStrLn ("Hello, " ++ name)
