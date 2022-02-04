module BoardBox where

import World.Objects
import World.Simulate

boardSelect :: Int -> Board
boardSelect num =
  case num of
    0 ->
      let robot = make (Robot Nothing) (4, 2)
          kids = makeMany Kid [(2, 0), (2, 4)]
          obstacles =
            makeMany
              Obstacle
              [ (1, 0),
                (2, 1),
                (3, 0),
                (1, 4),
                (2, 3),
                (3, 4)
              ]
          cribs = makeMany Crib [(0, 0), (0, 4)]
          objects = robot : (kids ++ obstacles ++ cribs)
       in worldInit 5 5 objects
    1 ->
      let robots = makeMany (Robot Nothing) [(11, 0), (11, 11)]
          kids = makeMany Kid ([(11, j) | j <- [4 .. 7]] ++ [(0, 5), (0, 6)])
          cribs = makeMany Crib ([(i, j) | i <- [0, 1], j <- [0, 11]] ++ [(0, 2), (0, 9)])
          obstacles =
            makeMany
              Obstacle
              ( [(i, j) | i <- [4, 8], j <- [0, 1, 10, 11]]
                  ++ [(i, j) | i <- [0 .. 4] ++ [8 .. 11], j <- [3, 8]]
              )
       in worldInit 12 12 (robots ++ kids ++ cribs ++ obstacles)
    11 ->
      let board1 = boardSelect 1
          dirts = makeMany Dirt [(6, j) | j <- [0 .. 11]]
       in board1 *++ dirts
    2 ->
      let robots = makeMany (Robot Nothing) [(i, 0) | i <- [8 .. 11]]
          kids = makeMany Kid [(i, j) | i <- [2 .. 9], let j = 3 + i]
          cribs = makeMany Crib [(i, 15) | i <- [0 .. 7]]
          obstacles =
            makeMany
              Obstacle
              ([(i, 1) | i <- [1 .. 11]] ++ [(i, 3) | i <- [0 .. 9]])
       in worldInit 12 16 (robots ++ kids ++ cribs ++ obstacles)
    22 ->
      let board3 = boardSelect 2
          dirts =
            makeMany
              Dirt
              ( (0, 1) :
                [(i, 0) | i <- [0 .. 11]]
                  ++ [(i, 2) | i <- [0 .. 11]]
                  ++ [(i, 3) | i <- [10 .. 11]]
              )
       in board3 *++ dirts
    3 ->
      let robots@[r1, r2, r3] = makeMany (Robot Nothing) [(7, 0), (7, 11), (4, 6)]
          cribs = makeMany Crib [(i, j) | i <- [0 .. 3], j <- [4 .. 7]]
          obstacles = makeMany Obstacle [(i, j) | j <- [0 .. 3] ++ [8 .. 11], i <- [8 .. 11]]
          kids = makeMany Kid [(i, j) | j <- [4 .. 7], i <- [8 .. 11]]
       in worldInit 12 12 (robots ++ cribs ++ obstacles ++ kids)
    33 ->
      let board3 = boardSelect 3
          cribs = makeMany Crib ([(i, j) | i <- [0 .. 3], j <- [3, 8]] ++ [(4, j) | j <- [4 .. 7]])
          kids = makeMany Kid ([(i, j) | i <- [0 .. 3], j <- [0, 11]] ++ [(6, j) | j <- [4 .. 7]])
       in board3 *++ (cribs ++ kids)
    _ -> error ("No board defined for num " ++ show num)
