import           Data.Text (unpack)
import           Data.List (sortOn)
import           Lib

main :: IO ()
main = do
  mod <- parseModuleFromFile "test/Test1.purs"
  putStrLn ""
  putStrLn $ show mod
  putStrLn ""
  putStrLn "% facts"
  putStrLn . unpack . formatAtomsProlog . sortForProlog . factsFromModule $ mod
  putStrLn ""
  putStrLn "% rules"
  putStrLn . unpack . formatRulesProlog $ definedInStar
  -- putStrLn ""
  -- putStrLn $ (formatDatomic . factsFromModule $ mod)
  where
    sortForProlog = sortOn (\(Pred name _) -> name)
