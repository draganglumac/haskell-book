module ChapterExercises where

import Moi

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put a = Moi $ const ((), a)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify sa = Moi $ \s -> ((), sa s)

main :: IO ()
main = do
  print (runMoi get "curryIsAmaze")             -- ("curryIsAmaze", "curryIsAmaze")
  print (runMoi (put "blah") "woot")            -- ((), "blah")
  print (exec (put "wilma") "daphne")           -- "wilma"
  print (exec get "scooby papu")                -- "scooby papu"
  print (eval get "bunnicula")                  -- "bunnicula"
  print (eval (put "wilma") "daphne")           -- ()
  print (runMoi (modify (+1)) 0)                -- ((), 1)
  print (runMoi (modify (+1) >> modify (+1)) 0) -- ((), 2) except this prints ((), 1)
                                                -- something wrong with my Monad maybe?
