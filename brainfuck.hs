data State =
    State { code      :: String,
            tape      :: [Integer],
            pointer   :: Integer,
            output    :: String
          }

adjustTapeValue :: State -> Integer -> State
adjustTapeValue (State code tape tp output) adjustment =
    State code tape' tp output
  where
    (ys, zs) = splitAt ((fromInteger tp) - 1) tape
    tmp = adjustment + head zs
    tape' = ys ++ [tmp] ++ (tail zs)

moveTape :: State -> (Integer -> Bool) -> Integer -> Integer -> State
moveTape (State code tape tp output) condition offset _default =
    State code tape tp' output
  where
    tp' = if condition tp then tp + offset else _default

step :: State -> State
step state@(State "" tape tp output) = state
step oldState@(State (command:code) tape tp output) =
    step $ case command of
      '>' -> moveTape state (< 1024)   1    0
      '<' -> moveTape state (> 0)    (-1) 1024
      
      '+' -> adjustTapeValue state   1
      '-' -> adjustTapeValue state (-1)
      {-
      '.' -> ???
      ',' -> ???
      -}
      _   -> state
  where
    state = State code tape tp output


brainfuck :: String -> String
brainfuck code =
    output $ step $ State code tape 0 ""
  where
    tape = replicate 1024 0

main :: IO ()
main = do
  -- "Hello World!\n"
  putStrLn $ brainfuck "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
