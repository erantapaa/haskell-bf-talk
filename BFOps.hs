module BFOps
  ( Instr(..)
  , simplify
  , toOpcodes
  )
where

data Instr = Move Int | Inc Int | Read | Print | Open | Close | Debug String
                deriving (Read, Show, Eq)

-- a peephole optimizer
simplify :: [Instr] -> [Instr]
simplify [] = []
simplify (Move 0 : xs) = simplify xs
simplify (Move a : Move b : xs) = simplify (Move (a+b) : xs)
simplify (Inc 0 : xs) = simplify xs
simplify (Inc a : Inc b : xs) = simplify (Inc (a+b) : xs)
simplify (x : xs) = x : simplify xs

-- translate a list of Instr to a String
toOpcodes :: [Instr] -> String
toOpcodes xs = concatMap go xs
  where go (Move x)
          | x > 0 = replicate x '>'
          | x < 0 = replicate (-x) '<'
          | otherwise = ""
        go (Inc x)
          | x > 0 = replicate x '+'
          | x < 0 = replicate (-x) '-'
          | otherwise = ""
        go Open = "["
        go Close = "]"
        go Read = ","
        go Print = "."
        go (Debug x)
            | ok x = "!" ++ x ++ "!"
            | otherwise = error $ "bad debug message: " ++ x
           where ok x = all (\c -> notElem c x) "![]<>+-.,"

