{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import Data.Foldable
import Data.Text hiding (map, foldl')
import Data.Char
import Data.String

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad

import Types

newtype Custom = Disallowed Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent Custom where
    showErrorComponent (Disallowed s) = "\"" ++ unpack s ++ "\" is not allowed here"

type Parser = Parsec Custom Text

isOpChar c = not (isSpace c) && not (isAlphaNum c) && (c `notElem`
    [ '"'
    , '\''
    , '(', ')'
    , '[', ']'
    , '{', '}'
    , '`'
    , '.'
    ])

disallowed =
    [ "forall"
    , "exists"
    , "lambda"
    , "let"
    , "in"
    , "∀"
    , "∃"
    , "=>"
    ]

many1 p = (:) <$> p <*> many p

trySum l = asum (map try l)

lexeme :: forall a. Parser a -> Parser a
lexeme p = do
    x <- p
    space
    return x

syntax :: String -> Parser Text
syntax = lexeme . string . pack

parens :: forall a. Parser a -> Parser a
parens p = do
    syntax "("
    x <- p
    syntax ")"
    return x

kindStar :: Parser Kind
kindStar = do
    syntax "*"
    return KindStar

kindArrow :: Parser Kind
kindArrow = do
    kindStar
    syntax "->"
    KindArrow <$> kind

kind :: Parser Kind
kind = trySum
    [ kindArrow
    , kindStar
    ]

disallow :: [Text] -> Parser Text -> Parser Text
disallow l p = do
    x <- p
    when (x `elem` l) (customFailure (Disallowed x))
    return x

suffix :: Parser Text
suffix = pack <$> many (alphaNumChar <|> char '_')

variable :: Parser Text
variable = disallow disallowed (lexeme (cons <$> lowerChar <*> suffix))

constructor :: Parser Text
constructor = disallow disallowed (lexeme (cons <$> upperChar <*> suffix))

operator :: Parser Text
operator = disallow disallowed (lexeme (pack <$> many1 (satisfy isOpChar)))

typeExpr :: Int -> Parser TypeExpr
typeExpr 0 = trySum
    [ do
        x <- typeExpr 1
        op <- operator
        y <- typeExpr 0
        return (TypeCon op [x, y])
    , typeExpr 1
    ]
typeExpr 1 = trySum
    [ do
        c <- constructor
        args <- many (typeExpr 2)
        return (TypeCon c args)
    , typeExpr 2
    ]
typeExpr 2 = trySum
    [ TypeVar <$> variable
    , flip TypeCon [] <$> constructor
    , parens (typeExpr 0)
    ]

typePred :: Parser TypePred
typePred = trySum
    [ Pred <$> constructor <*> many1 (typeExpr 1)
    , do
        left <- typeExpr 1
        syntax "<="
        right <- typeExpr 0
        return (left :<= right)
    , do
        left <- typeExpr 1
        syntax ">="
        right <- typeExpr 0
        return (left :>= right)
    ]

qualType :: Parser QualType
qualType = trySum
    [ do
        pred <- typePred
        syntax "=>"
        Qual pred <$> typeExpr 0
    , TypeExpr <$> typeExpr 0
    ]

typeScheme :: Parser TypeScheme
typeScheme = trySum
    [ QualType <$> qualType
    , do
        syntax "∀" <|> syntax "forall"
        v <- variable
        syntax "."
        ForAll v <$> typeScheme
    ]

expr :: Int -> Parser Expr
expr 0 = trySum
    [ do
        x <- expr 1
        op <- operator
        y <- expr 0
        return (App (App (Var op) x) y)
    , expr 1
    ]
expr 1 = do
    x : xs <- many1 (expr 2)
    return (foldl' App x xs)
expr 2 = trySum
    [ Var <$> variable
    , parens (expr 0)
    ]

parse :: forall e. Text -> Either (ParseErrorBundle Text e) Module
parse = runParser undefined "module"