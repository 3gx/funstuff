{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module QQAst (module QQAst)
             where


import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Data (Data)
--import Data.Generics (extQ)
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

type VarName = String

data Expr = Var VarName
          | Add Expr Expr 
          | Sub Expr Expr
          | Int Integer
          | Read
    deriving (Data, Typeable, Show, Eq)

data Cmd = Write Expr
         | Assign VarName Expr
         | Decl VarName
  deriving (Data, Typeable, Show)

data Prog = Prog [Cmd] deriving (Data, Typeable, Show)

prog :: QuasiQuoter
prog = QuasiQuoter {
      quoteExp = \str -> do
        l <- location'
        c <- runIO $ parseIO (setPosition l *> topLevel parseProg) str
        dataToExpQ (const Nothing) c
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

parseIO :: Parser a -> String -> IO a
parseIO p str =
  case parse p "" str of
    Left err -> throwIO (userError (show err))
    Right a  -> return a
