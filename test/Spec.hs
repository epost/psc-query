import           Data.List (sortOn)
import           Language.PureScript.AST.Declarations
import           Lib

main :: IO ()
main = do
  mod <- parseModuleFromFile "test/Test1.purs"
  putStrLn ""
  putStrLn $ show mod
  putStrLn ""
  putStrLn "% facts"
  putStrLn . formatAtomsProlog . sortForProlog . factsFromModule $ mod
  putStrLn ""
  putStrLn "% rules"
  putStrLn $ formatRulesProlog definedInStar
  -- putStrLn ""
  -- putStrLn $ (formatDatomic . factsFromModule $ mod)
  where
    sortForProlog = sortOn (\(Pred name _) -> name)
