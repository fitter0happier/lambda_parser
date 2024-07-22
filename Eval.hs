module Eval where

type Symbol = String
data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var x) = x
    show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
    show (Lambda x y) = "(\\" ++ x ++ "." ++ show y ++ ")"

free :: Expr -> Symbol -> Bool
free (Var x) sym | x == sym = True
                 | otherwise = False
free (App x y) expr = free x expr || free y expr
free (Lambda x body) sym = not (free (Var x) sym) && free body sym

sub :: Expr -> Symbol -> Expr -> Int -> Expr
--x[x:=e] = e
--y[x:=e] = y, y /= x
sub (Var x) sym e _ | x == sym = e                                                       
                    | otherwise = Var x     

--(e1 e2)[x:=e] = (e1[x:=e] e2[x:=e])
sub (App e1 e2) sym e counter = App (sub e1 sym e counter) (sub e2 sym e counter)  

--(Lambda x.e')[x:=e] = (Lambda x.e')
--(Lambda y.e')[x:=e] = (Lambda y.e'[x:=e]), y /= x && y is not free in e   
--(Lambda y.e')[x:=e] = (Lambda z.e'[y:=z][x:=e]), y /= x && y is free in e; z is a fresh variable       
sub (Lambda x e') sym e counter | x == sym = Lambda x e'  
                                | not (free e x) = Lambda x (sub e' sym e counter)                     
                                | otherwise = Lambda fresh (sub (sub e' x (Var fresh) (counter + 1)) sym e (counter + 1)) where
                                  fresh = "a" ++ show counter
    
redex :: Expr -> Expr -> Expr
redex (Lambda x orig_expr) expr_to_sub = sub orig_expr x expr_to_sub 0 

reduce :: Expr -> Expr
reduce (Var x) = Var x
reduce (Lambda x expr) = Lambda x (reduce expr)
reduce (App (Lambda x expr) y) = redex (Lambda x expr) y
reduce (App x y) = App (reduce x) (reduce y)

eval :: Expr -> Expr
eval expr = let expr' = reduce expr in if expr' /= expr then eval expr' else expr'

ex = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))

one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))
suc = Lambda "w"
       (Lambda "y"
         (Lambda "x"
           (App (Var "y")
                (App (App (Var "w") (Var "y"))
                     (Var "x")))))    

ex2 = App (Lambda "x"
           (Lambda "y"
             (Lambda "z" (App (App (Var "x") (Var "y")) (Var "z")))))
         (App (Var "y") (Var "z"))
