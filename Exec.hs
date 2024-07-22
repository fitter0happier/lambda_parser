import Eval
import LambdaParser

main :: IO ()
main = do 
    inp <- getContents
    case readPrg inp of
        Nothing -> putStrLn "Incorrect program"
        Just e -> print $ eval e
