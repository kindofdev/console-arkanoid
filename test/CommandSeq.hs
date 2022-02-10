module CommandSeq where

import Arkanoid.Types ( Command(..) )

ticksAndQuit :: Int -> [Command]
ticksAndQuit n = replicate n Tick <> [Quit]  

playerMoveAndQuit :: [Command]
playerMoveAndQuit = mconcat
  [ [Tick]
  , [MoveRightPaddle]
  , [MoveRightPaddle]
  , [MoveRightPaddle]
  , [MoveRightPaddle]
  , [MoveRightPaddle]
  , [MoveLeftPaddle]
  , [Tick]
  , [Quit]
  ]

pauseUnpauseAndMore :: [Command]
pauseUnpauseAndMore = mconcat 
  [ replicate 10 Tick
  , [Pause]
  , [MoveRightPaddle] 
  , replicate 10 Tick
  , [Pause]  -- unpause
  , replicate 10 Tick
  , [Quit]
  ] 