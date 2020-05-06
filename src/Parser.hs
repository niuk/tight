{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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

type Parser = Parsec String Text

isOpChar c = not (isSpace c) && not (isAlphaNum c) && (c `notElem`
    [ '"'
    , '\''
    , '(', ')'
    , '[', ']'
    , '{', '}'
    , '`'
    , '.'
    , ','
    , ':'
    , ';'
    ])

special =
    [ "forall"
    , "exists"
    , "type"
    , "∀"
    , "∃"
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
parens = between (syntax "(") (syntax ")")

brackets :: forall a. Parser a -> Parser a
brackets = between (syntax "[") (syntax "]")

braces :: forall a. Parser a -> Parser a
braces = between (syntax "{") (syntax "}")

kindExpr :: Int -> Parser KindExpr
kindExpr 0 = trySum
    [ do
        x <- kindExpr 1
        syntax "->"
        KindArrow x <$> kindExpr 0
    , kindExpr 1
    ]
kindExpr 1 = trySum
    [ do
        syntax "*"
        return KindStar
    , parens (kindExpr 0)
    ]

exclude :: [Text] -> Parser Text -> Parser Text
exclude l p = do
    x <- p
    when (x `elem` l) (fail ("\"" ++ unpack x ++ "\" is not allowed here."))
    return x

suffix :: Parser Text
suffix = pack <$> many (alphaNumChar <|> char '_')

variable :: Parser Text
variable = exclude special (lexeme (cons <$> lowerChar <*> suffix))

constructor :: Parser Text
constructor = exclude special (lexeme (cons <$> upperChar <*> suffix))

operator :: Parser Text
operator = exclude special (lexeme (pack <$> many1 (satisfy isOpChar)))

operatorExpr :: forall a. Parser a -> Parser (Either Text a)
operatorExpr p = trySum
    [ do
        syntax "`"
        x <- p
        syntax "`"
        return (Right x)
    , Left <$> operator
    ]

typeExpr :: Int -> Parser TypeExpr
typeExpr 0 = trySum
    [ do
        syntax "∀" <|> syntax "forall"
        vs <- many1 variable
        syntax ":"
        ForAll vs <$> typeExpr 0
    , do
        syntax "∃" <|> syntax "exists"
        vs <- many1 variable
        syntax ":"
        Exists vs <$> typeExpr 0
    , EffectType <$> brackets (sepBy (typeExpr 0) (syntax ",")) <*> typeExpr 1
    , do
        x <- typeExpr 1
        op <- either id id <$> operatorExpr variable
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

pattern :: Int -> Parser Pattern
pattern 0 = trySum
    [ do
        c <- constructor
        MatchCon c <$> many (pattern 1)
    , pattern 1
    ]
pattern 1 = trySum
    [ Bind <$> variable
    , flip MatchCon [] <$> constructor
    ]

def :: Parser Def
def = do
    p <- pattern 0
    syntax "="
    Def p <$> expr 0

lambdaBody :: Parser [Either Def Expr]
lambdaBody = sepBy1 (trySum
    [ Left <$> def
    , Right <$> expr 0
    ]) (syntax ",")

lambdaCase :: Parser ([Pattern], [Either Def Expr])
lambdaCase = trySum
    [ do
        ps <- many1 (pattern 1)
        syntax ":"
        (ps,) <$> lambdaBody
    , ([],) <$> lambdaBody
    ]

expr :: Int -> Parser Expr
expr 0 = trySum
    [ do
        syntax "["
        cs <- sepBy1 lambdaCase (syntax ";")
        syntax "]"
        return (Lambda cs)
    , do
        syntax "Lambda" <|> syntax "Λ"
        v <- variable
        TypeLambda v <$> lambdaBody
    , do
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

parse :: Text -> Either (ParseErrorBundle Text String) [Def]
parse = runParser (sepBy1 def (syntax ",")) "module"