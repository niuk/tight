module Types where

import Data.Text

data KindExpr
    = KindArrow KindExpr KindExpr
    | KindStar
    deriving Show

data Row
    = Field Text TypeExpr
    | Spread TypeExpr
    deriving Show

data TypeExpr
    = ForAll [Text] TypeExpr
    | Exists [Text] TypeExpr
    | TypeCon Text [TypeExpr]
    | Qual [Row] Text TypeExpr
    | RecType [Row] Text
    | TypeVar Text
    deriving Show

data Pattern
    = Bind Text
    | BindWith Text Pattern
    | MatchCon Text [Pattern]
    | MatchRec [(Text, Pattern)] Text
    deriving Show

data Def
    = Def Pattern Expr
    | DefCon Text TypeExpr
    | DefType Text KindExpr
    deriving Show

data Expr
    = Var Text
    | App Expr Expr
    | TypeApp Expr TypeExpr
    | Lambda [([Pattern], [Either Def Expr])]
    | TypeLambda Text [Either Def Expr]
    | Rec [Def] [Expr]
    | Selection [Def]
    | Tuple [Expr]
    | Nil
    deriving Show