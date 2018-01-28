{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, unpack, toUpper)
import           Language.PureScript.AST.Declarations hiding (Var)
import           Language.PureScript.AST.SourcePos (SourceSpan(..), SourcePos(..))
import qualified Language.PureScript.Environment as PE
import           Language.PureScript.Names (ProperName(..), Ident(..), ModuleName(..))
import qualified Language.PureScript.Parser as P

-- TODO repeating 'Pred XxxP' instead of 'XxxP' seems a bit wordy
-- TODO add Rule [Var] constructor
data Atom a = Pred PredName [Term a] deriving Functor

data Term a = Con a | Var a deriving Functor

-- TODO iso (Atom a, Atom a)
data Rule a = Rule { ruleHead :: Atom a, ruleBody :: [Atom a] } deriving Functor

data PredName = ModP
              | DatP
              | NewP
              | ValP
              | ConP
              | DefinedInP
              | DefinedInStarP
                deriving (Eq, Ord)

type ModuleId = Text


-- TODO model ConP (i.e. data constructors belonging under a certain data decl) using DefinedInP, or some other containment relationship
factsFromModule :: Module -> [Atom Text]
factsFromModule (Module ss cmt (ModuleName properNames) decls _) =
     (Pred ModP [Con moduleName])
   : (map2 moduleFacts fqn)
  ++ (factsFromDecl moduleName =<< decls)
  where
    fqn = runProperName <$> reverse properNames
    moduleName = head fqn -- TODO unsafe, clean up
    moduleFacts child parent = [Pred DefinedInP [Con child, Con parent], Pred ModP [Con parent]]

factsFromDecl :: ModuleId -> Declaration -> [Atom Text]
factsFromDecl modId decl = case decl of
  DataDeclaration sourceAnn dataDeclType (ProperName name) _ ctors ->
    [ Pred (predicateFromDataDeclType dataDeclType) [Con name]
    , Pred DefinedInP [Con name, Con modId]
    ] ++
    (Pred ConP . (:[Con name]) <$> Con . runProperName . fst <$> ctors)
  ValueDeclaration (ValueDeclarationData sourceAnn (Ident ident) name binders expr) ->
    [ Pred ValP [Con ident]
    , Pred DefinedInP [Con ident, Con modId]
    ]
  _ ->
    []

predicateFromDataDeclType PE.Data    = DatP
predicateFromDataDeclType PE.Newtype = NewP

--------------------------------------------------------------------------------

-- defined_in*(X,Y) :- defined_in(X,Y)
--                  :- defined_in(X,Z), defined_in*(Z,Y)
definedInStar :: [Rule Text]
definedInStar =
  let head = Pred DefinedInStarP [Var "x", Var "y"]
  in [ Rule head [ Pred DefinedInP [Var "x", Var "y"]]
     , Rule head [ Pred DefinedInP [Var "x", Var "z"]
                 , Pred DefinedInStarP [Var "z", Var "y"]
                 ]
     ]

-- formatters for various datalog/prolog dialects ------------------------------

-- Prolog format is used by several datalog varieties.
formatAtomsProlog :: [Atom Text] -> Text
formatAtomsProlog facts = (mkString ".\n" (formatAtomProlog <$> facts)) <> "."

formatAtomProlog :: Atom Text -> Text
formatAtomProlog (Pred name xs) = formatPredNameProlog name <> "(" <> mkString ", " (formatTerm <$> xs) <> ")"

formatRulesProlog :: [Rule Text] -> Text
formatRulesProlog rules = mkString "\n" (formatRule <$> rules)
  where
    formatRule (Rule head body) = formatAtomProlog head <> " :- " <> mkString ", " (formatAtomProlog <$> body) <> "."

-- Datomic and Datascript formatters
formatDatomic :: [Atom Text] -> Text
formatDatomic facts = "[\n  " <> mkString "\n  " (formatAtom <$> facts) <> "\n]"
  where
    formatAtom :: Atom Text -> Text
    formatAtom (Pred name xs) = ":" <> formatPredNameProlog name <> " " <> (mkString " " (formatTerm <$> xs))

formatTerm :: Term Text -> Text
formatTerm (Con s) = "\"" <> s <> "\""
formatTerm (Var s) = toUpper s


formatPredNameProlog :: PredName -> Text
formatPredNameProlog ModP           = "module"
formatPredNameProlog DatP           = "data"
formatPredNameProlog ConP           = "data_ctor"
formatPredNameProlog NewP           = "newtype"
formatPredNameProlog ValP           = "value"
formatPredNameProlog DefinedInP     = "defined_in"
formatPredNameProlog DefinedInStarP = "defined_in_star"

-- TODO get rid of the packing and unpacking
mkString :: Text -> [Text] -> Text
mkString sep ts = pack $ intercalate (unpack sep) (unpack <$> ts)

--------------------------------------------------------------------------------

map2 :: (a -> a -> [b]) -> [a] -> [b]
map2 = map2' []
  where
    map2' :: [b] -> (a -> a -> [b]) -> [a] -> [b]
    map2' bs f []           = bs
    map2' bs f [_]          = bs
    map2' bs f (a0:a1:mods) = bs <> map2' (f a0 a1) f (a1:mods)

--------------------------------------------------------------------------------

-- courtesy of kritzcreek/psc-ide
parseModuleFromFile :: FilePath -> IO Module
parseModuleFromFile fp = do
  content <- readFile fp
  Right m <- return $ do
    tokens <- P.lex fp (pack content)
    P.runTokenParser "" P.parseModule tokens
  return m
