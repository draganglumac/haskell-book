module MonadLaws where

monadLaws :: IO ()
monadLaws = do
  putStrLn "Left-identity law:  m >>= return == m"
  putStrLn "Right-identity law: return x >>= f == f x"
  putStrLn ""
  putStrLn "Both identity laws say that `return` should be neutral and perform no computation."
  putStrLn ""
  putStrLn "Associativity law:  (m >>= f) >>= g == m >>= (\\x -> f x >>= g)"
