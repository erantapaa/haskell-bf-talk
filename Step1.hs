module Step1 where

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

compile = toOpcodes . simplify

-- print the letter A
ex1 = [ Inc 10    -- set cell 0 to 10
      , Open
      ,   Move 1, Inc 6        -- increment cell 1 by 6
      ,   Move (-1), Inc (-1)  -- move back to 0 and decrement by 1
      , Close
      , Move 1, Inc 5          -- add 5 to cell 1
      , Print
      ]

ex1' = compile ex1

-- clear cell x
clear x = [ Move x, Open, Inc (-1), Close, Move (-x) ]

-- increment cell x by the constant a
incr_by x a = [ Move x, Inc a, Move (-x) ]

-- perform body the number of times in cell x
dotimes' x body = [ Move x, Open, Move (-x) ]
                    ++ body
                    ++ [ Move x, Inc (-1), Close, Move (-x) ]

at x body = [ Move x ] ++ body ++ [ Move (-x) ]

-- print the letter 'A'
ex2 = incr_by 0 10
      ++ dotimes' 0 (incr_by 1 6)
      ++ incr_by 1 5
      ++ at 1 [ Print ]
      ++ at 1 [ Debug "" ]

-- :add BFInterp
-- :m +Step1
-- runBF $ compile ex1

