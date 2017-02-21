{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module QQAst where

import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Data (Data)
import Data.Generics (extQ)
import Data.IORef
import Data.Set (Set)
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import Text.Parsec.Pos
import qualified Data.Set          as Set
import qualified Text.Parsec.Token as P

{-------------------------------------------------------------------------------
  AST
-------------------------------------------------------------------------------}

type VarName = String

data Expr =
    Var VarName
  | Add Expr Expr
  | Sub Expr Expr
  | Int Integer
  | Read
  deriving (Data, Typeable, Show, Eq)

data Cmd =
    Write Expr
  | Assign VarName Expr
  | Decl VarName
  deriving (Data, Typeable, Show)

data Prog = Prog [Cmd]
  deriving (Data, Typeable, Show)

{-------------------------------------------------------------------------------
  Overloading expressions
-------------------------------------------------------------------------------}

class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr VarName where
  toExpr = Var

instance ToExpr Integer where
  toExpr = Int

{-------------------------------------------------------------------------------
  Lexer
-------------------------------------------------------------------------------}

lexer = P.makeTokenParser haskellStyle {
      P.reservedNames   = ["write", "read", "var"]
    , P.reservedOpNames = ["+", "-", ";", ":="]
    }

identifier = P.identifier lexer
integer    = P.integer    lexer
parens     = P.parens     lexer
reserved   = P.reserved   lexer
reservedOp = P.reservedOp lexer
whiteSpace = P.whiteSpace lexer

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseVarName :: Parser VarName
parseVarName = identifier

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table term
  where
    term = choice [
        parens parseExpr
      , Read <$ reserved "read"
      , Var <$> parseVarName
      , Int <$> integer
      ]

    table = [[ Infix (Add <$ reservedOp "+") AssocLeft
             , Infix (Sub <$ reservedOp "-") AssocLeft
             ]]

parseCmd :: Parser Cmd
parseCmd = choice [
      Write <$ reserved "write" <*> parseExpr
    , Decl <$ reserved "var" <*> parseVarName
    , Assign <$> parseVarName <* reservedOp ":=" <*> parseExpr
    ]

parseProg :: Parser Prog
parseProg = Prog <$> parseCmd `sepBy1` reservedOp ";"

parseIO :: Parser a -> String -> IO a
parseIO p str =
  case parse p "" str of
    Left err -> throwIO (userError (show err))
    Right a  -> return a

topLevel :: Parser a -> Parser a
topLevel p = whiteSpace *> p <* eof

{-------------------------------------------------------------------------------
  Free variables
-------------------------------------------------------------------------------}

fvExpr :: Expr -> Set VarName
fvExpr (Var x)     = Set.singleton x
fvExpr (Add e1 e2) = fvExpr e1 `Set.union` fvExpr e2
fvExpr (Sub e1 e2) = fvExpr e1 `Set.union` fvExpr e2
fvExpr Read        = Set.empty
fvExpr (Int _)     = Set.empty

fvCmds :: [Cmd] -> Set VarName
fvCmds []                = Set.empty
fvCmds (Write e    : cs) = Set.unions [fvExpr e, fvCmds cs]
fvCmds (Assign x e : cs) = Set.unions [Set.singleton x, fvExpr e, fvCmds cs]
fvCmds (Decl x     : cs) = Set.delete x (fvCmds cs)

fvProg :: Prog -> Set VarName
fvProg (Prog cmds) = fvCmds cmds

{-------------------------------------------------------------------------------
  Quasi-quoters
-------------------------------------------------------------------------------}

expr :: QuasiQuoter
expr = QuasiQuoter {
      quoteExp  = \str -> do
        l <- location'
        e <- runIO $ parseIO (setPosition l *> topLevel parseExpr) str
        dataToExpQ (const Nothing `extQ` metaExp (fvExpr e)) e
    , quotePat  = \str -> do
        l <- location'
        e <- runIO $ parseIO (setPosition l *> topLevel parseExpr) str
        dataToPatQ (const Nothing `extQ` metaPat (fvExpr e)) e
    , quoteType = undefined
    , quoteDec  = undefined
    }

prog :: QuasiQuoter
prog = QuasiQuoter {
      quoteExp = \str -> do
        l <- location'
        c <- runIO $ parseIO (setPosition l *> topLevel parseProg) str
        dataToExpQ (const Nothing `extQ` metaExp (fvProg c)) c
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

metaExp :: Set VarName -> Expr -> Maybe ExpQ
metaExp fvs (Var x) | x `Set.member` fvs = Just [| toExpr $(varE (mkName x)) |]
metaExp _ _ = Nothing

metaPat :: Set VarName -> Expr -> Maybe PatQ
metaPat fvs (Var x) | x `Set.member` fvs = Just (varP (mkName x))
metaPat _ _ = Nothing

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

newtype Interpreter a = Interpreter {
    runInterpreter :: StateT [(VarName, IORef Integer)] IO a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState [(VarName, IORef Integer)]
           )

intVarName :: VarName -> Interpreter (IORef Integer)
intVarName x = do
  mref <- lookup x <$> get
  case mref of
    Nothing  -> liftIO $ throwIO $ userError ("Unbound variable " ++ show x)
    Just ref -> return ref

intExpr :: Expr -> Interpreter Integer
intExpr (Var x)     = liftIO . readIORef =<< intVarName x
intExpr (Add e1 e2) = (+) <$> intExpr e1 <*> intExpr e2
intExpr (Sub e1 e2) = (-) <$> intExpr e1 <*> intExpr e2
intExpr Read        = liftIO $ readLn
intExpr (Int n)     = return n

intCmd :: Cmd -> Interpreter ()
intCmd (Write e) = do
  n <- intExpr e
  liftIO $ print n
intCmd (Assign x e) = do
  ref <- intVarName x
  n   <- intExpr e
  liftIO $ writeIORef ref n
intCmd (Decl x) = do
  ref <- liftIO $ newIORef 0
  modify ((x, ref) :)

intProg :: Prog -> Interpreter ()
intProg (Prog cmds) = mapM_ intCmd cmds

intIO :: Interpreter a -> IO a
intIO i = evalStateT (runInterpreter i) []
