module Arkanoid.Types where

import Data.Map (Map)
import System.Console.ANSI ( Color )

type Width  = Int
type Height = Int

type PositionX = Int
type PositionY = Int

type WindowHeight = Height
type WindowWidth  = Width

type TerminalSize = (Height, Width)

type PaddleWidth = Width
type PaddleStep  = Int

type PlayerPosition       = Int
type OriginPlayerPosition = PlayerPosition
type FinalPlayerPosition  = PlayerPosition

type BallPositionX = Float
type BallPositionY = Float
type BallPosition  = (BallPositionX, BallPositionY)
type VelX          = Int 
type VelY          = Int 
type BallVel       = (VelX, VelY)
type OriginBallVel = BallVel
type FinalBallVel  = BallVel

-- | A map containing all bricks which are structured by 'PositionY'.
type BrickMap = Map PositionY [Brick]

-- | Remaining hits to break a brick.
data Hits = OneH 
          | TwoH 
          | ThreeH 
          deriving (Eq, Show, Enum, Ord)

-- | A brick. 
data Brick    = Brick 
  { brx   :: PositionX
  , brw   :: Width
  , color :: Color
  , hits  :: Hits
  } deriving (Eq, Show)

-- | Frames per second.
type FPS        = Int
type GamePaused = Bool

type Score  = Int
type Lives  = Int

type DifficultyLevel = Int

data ArkanoidResult = PlayerWin Score 
                    | PlayerLose Score 
                    | Quitted
                    deriving (Show, Eq) 

data Command = MoveRightPaddle
             | MoveLeftPaddle
             | Pause
             | Tick
             | Quit
             deriving (Show, Eq)

data Event = GameStarted
           | BallMoved BallPosition
           | PaddleMovedLeft OriginPlayerPosition FinalPlayerPosition
           | PaddleMovedRight OriginPlayerPosition FinalPlayerPosition
           | WallCollision OriginBallVel FinalBallVel
           | PaddleCollision OriginBallVel FinalBallVel
           | BrickCollision OriginBallVel FinalBallVel
           | GamePaused
           | GameUnPaused
           | PlayerWon Score
           | PlayerLost Score
           | GameTerminated
           | GameQuitted
           deriving (Eq, Show)             

data InputError = WindowSizeError 
                | PaddleSizeError
                | FPSError
                | PlayerPositionError
                | BallPositionError
                | BallVelocityError
                | LivesError
                | DifficultyError
                deriving (Eq, Show)