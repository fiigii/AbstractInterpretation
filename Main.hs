module Main where

import System.IO
import LabeledAst
import AbstractInterpreter

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = withFile
       "test/twice.js"
       ReadMode
       (\handle -> do
           source <- hGetContents handle
           let (ast, cache, env) = k_CFA source
           putStrLn "Program:"
           print ast
           putStrLn "\nCache:"
           mapM_ (\(l, x) -> putStrLn $ show l ++ " : " ++
                             (Map.foldlWithKey (\acc k v -> acc ++ " at " ++ show k ++ " -> " ++ show (Set.toList v))
                             "\n" x)) $ Map.toList cache
           putStrLn "\nEnvir:"
           mapM_ (\((x, d), vs) -> putStrLn $ x ++ " at " ++ show d ++ " : " ++ show (Set.toList vs)) $ Map.toList env
       )
            
