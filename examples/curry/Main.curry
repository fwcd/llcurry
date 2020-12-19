main :: IO ()
main = do
    s <- return "Hello world!"
    putStrLn s
