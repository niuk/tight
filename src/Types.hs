module Types where

import Data.Text

data KindExpr
    = KindArrow KindExpr KindExpr
    | KindCross
    | KindStar
    deriving Show

data Row
    = Field Text TypeExpr
    | Spread TypeExpr
    deriving Show

data TypeExpr
    = ForAll [Text] TypeExpr
    | Exists [Text] TypeExpr
    | EffectType [TypeExpr] TypeExpr
    | TypeCon Text [TypeExpr]
    | Qual [Row] Text TypeExpr
    | RecordType [Row] Text
    | ChoiceType [Row] Text
    | TupleType [TypeExpr]
    | SelectionType [TypeExpr]
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
    | Record [Either Def Expr]
    | Choice [(Maybe Text, Expr)]
    | Tuple [Expr]
    | Selection [Expr]
    | Nil
    deriving Show