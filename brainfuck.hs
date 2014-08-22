#!/usr/bin/env ruby

-- Brainfuck interpreter!

data State =
    State { code        :: String,
            tape        :: [Integer],
            pointer     :: Integer,
            output      :: String,
            code_index  :: Integer,
            done        :: Bool
          }

adjustTapeValue :: State -> Integer -> State
adjustTapeValue (State code tape tp output idx done) adjustment =
    State code tape' tp output idx done
  where
    (ys, zs) = splitAt ((fromInteger tp) - 1) tape
    tmp = (tape !! (fromInteger tp)) + adjustment
    tape' = ys ++ [tmp] ++ zs

moveTape :: State -> (Integer -> Bool) -> Integer -> Integer -> State
moveTape (State code tape tp output idx done) condition offset _default =
    State code tape tp' output idx done
  where
    tp' = if condition tp then tp + offset else _default

getCommand :: State -> Char
getCommand (State code _ _ _ idx _) = code !! (fromInteger idx)

step :: State -> State
step (State code tape tp output idx done) =
    execute (getCommand state) state
  where
    isDone = ((fromInteger idx) >= ((length code) - 1))
    state = State code tape tp output (idx + 1) isDone

execute :: Char -> State -> State
execute '>' state = step $ moveTape state (\tp -> tp < 1024)   1  0
execute '<' state = step $ moveTape state (\tp -> tp > 0)    (-1) 1024

execute '+' state = step $ adjustTapeValue state   1
execute '-' state = step $ adjustTapeValue state (-1)

{-
execute '.' state = ...
execute ',' state = ...

execute '[' state = ...
execute ']' state = ...
-}

execute  _  (State code tape tp output idx False) =
    step $ State code tape tp output idx False

execute  _  (State code tape tp output idx True)  = State code tape tp output idx True


brainfuck :: String -> String
brainfuck code =
    output $ step $ State code tape 0 "" 0 False
  where
    tape = replicate 1024 0

main :: IO ()
main = do
  -- "Hello World!\n"
  putStrLn $ brainfuck "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
