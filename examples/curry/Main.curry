-- Compiles to LLVM IR, but does not run yet due to missing
-- Prelude implementation.

main :: IO ()
main = do
    s <- return "Hello world!"
    putStrLn s
