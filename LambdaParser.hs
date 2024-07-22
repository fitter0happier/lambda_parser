module LambdaParser where

import Control.Applicative
import Data.Char
import Parser ( alphaNum, char, sat, sep, string, Parser(parse) )
import Eval
import Data.Map (Map)
import qualified Data.Map as Map

-- <program> -> (<definition> <sep>)* <expr> <space>*

-- <definition> -> <var> <sep> ":=" <sep> <expr>

-- <expr> -> <var>
--         | "(\\" <var> '.' <expr> ')'
--         | '(' <expr> <sep> <expr> ')'

-- <var> -> <alphanum>+
-- <alphanum> -> any alphanumeric character recognized by isAlphaNum

-- <sep> -> <space>+
-- <space> -> any whitespace recognized by isSpace, e.g. ' ', '\n'


-- <definition> -> <var> <sep> ":=" <sep> <expr>
definition :: Parser (String, Expr)
definition = do
    v <- var
    sep
    string ":="
    sep
    e <- lambdaExpr
    return (getName v, e)
    where
       getName (Var name) = name 

-- <var> -> <alphanum>+
-- <alphanum> -> any alphanumeric character recognized by isAlphaNum
var :: Parser Expr 
var = do 
    x <- sat isAlphaNum
    xs <- many alphaNum
    return $ Var (x:xs)

-- <expr> -> "(\\" <var> '.' <expr> ')'
lambda :: Parser Expr
lambda = do
    string "(\\"
    v <- var
    char '.'
    e <- lambdaExpr
    char ')'
    return $ Lambda (getName v) e
    where
        getName (Var name) = name

-- <expr> -> '(' <expr> <sep> <expr> ')'
app :: Parser Expr
app = do
    char '('
    e1 <- lambdaExpr
    sep 
    e2 <- lambdaExpr
    char ')'
    return $ App e1 e2

-- parsing an expression
lambdaExpr :: Parser Expr
lambdaExpr = var <|> lambda <|> app

-- <program> -> (<definition> <sep>)* <expr> <space>*
program :: Parser (Map String Expr, Expr)
program = do
    defs <- many (definition <* sep)
    e <- lambdaExpr
    let env = foldl (\acc (k, v) -> Map.insert k (subs acc v) acc) Map.empty defs
    return (env, e)

substituteInDefinitions :: Map String Expr -> Map String Expr
substituteInDefinitions env = Map.map (subs env) env

-- substitute variables in an expression
subs :: Map String Expr -> Expr -> Expr
subs env (Var x) = Map.findWithDefault (Var x) x env
subs env (Lambda x e) = Lambda x (subs (Map.delete x env) e)
subs env (App e1 e2) = App (subs env e1) (subs env e2)

readPrg :: String -> Maybe Expr
readPrg x = do
    ((env, e), _) <- parse program x
    return $ subs env e
