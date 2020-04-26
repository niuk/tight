module Types where

import Data.Text

data Kind
    = KindStar
    | KindArrow Kind
  deriving Show

data TypeExpr
    = TypeVar Text
    | TypeCon Text [TypeExpr]
  deriving Show

data TypePred
    = Pred Text [TypeExpr]
    | TypeExpr :<= TypeExpr
    | TypeExpr :>= TypeExpr
  deriving Show

data QualType
    = TypeExpr TypeExpr
    | Qual TypePred TypeExpr
  deriving Show

data TypeScheme
    = QualType QualType
    | ForAll Text TypeScheme
  deriving Show

data Expr
    = Var Text
    | App Expr Expr
    | Lambda Text Expr
    | InLeft Expr
    | InRight Expr
    | Case Expr Text Expr Text Expr
    | LetVar Text Expr Expr
  deriving Show

data Module = Module [(Text, TypeExpr)] [(Text, Expr)]