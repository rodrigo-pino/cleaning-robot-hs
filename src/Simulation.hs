module Simulation where

runSimulation num = do
  putStrLn "running simmulation"

simulation times shuffleT = do
  putStrLn "aouch"

boardSelect num =
  case num of
    0 -> []
    1 -> []
    2 -> []
    _ -> error "No board defined for num " ++ show num
