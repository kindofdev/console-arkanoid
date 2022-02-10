module Cli ( parseArgsIO ) where

import Control.Monad ( forM )
import qualified Data.Map as M
import System.Console.ANSI ( Color(Red, Yellow), getTerminalSize )
import System.Environment (getArgs)
import System.Random ( randomRIO )

import Arkanoid.DSL ( ArkanoidState, Env, mkArkanoidState, mkEnv )
import Arkanoid.Types
    ( BallPosition,
      BallVel,
      Brick(Brick),
      BrickMap,
      DifficultyLevel,
      FPS,
      Lives,
      PaddleWidth,
      PlayerPosition,
      PositionY,
      TerminalSize,
      WindowHeight,
      WindowWidth )

-- | Collect args and parse them as 'Env' and 'ArkanoidState' (initialState)
parseArgsIO :: IO (Env, ArkanoidState)
parseArgsIO = getArgs >>= parseArgs

-- <windowWidth> <windowHeight> <paddleWidth> <fps> <ballVelocityX> <ballVelocityY> <lives> <difficulty> <logFile>
parseArgs :: [String] -> IO (Env, ArkanoidState)
parseArgs [ww, wh, pw, fps_, bvx, bvy, lvs, d, file] = do
    let ww'   = read ww
        wh'   = read wh
        pw'   = read pw
        fps'  = read fps_
        bvx'  = read bvx
        bvy'  = read bvy
        lvs'  = read lvs
        d'    = read d
        
        bv       = (bvy', negate bvx')
        p        = 1                          -- Player starts at the left side
        bpy      = fromIntegral wh' - 2       -- Ball starts on the paddle
        bpx      = fromIntegral $ p + pw' - 1    

    Just ts      <- getTerminalSize
    env          <- buildEnv ts ww' wh' pw' Red Yellow fps' d' (Just file)
    let dP = case d' of 
                  1 -> DifficultyParams 20 (0, 0)
                  2 -> DifficultyParams 30 (0, 1)   
                  3 -> DifficultyParams 40 (0, 2)
                  _ -> error "This can never happen" -- mkEnv takes care of it

    bricks       <- buildBricks ww' wh' dP
    initialState <- buildInitialState (bpx, bpy) bv p bricks lvs' env  
    return (env, initialState)
parseArgs _ = error "The CLI args could not be parsed"    

-- | Build an Env inside IO failing if the input params are wrong. 
buildEnv :: TerminalSize 
         -> WindowWidth 
         -> WindowHeight
         -> PaddleWidth
         -> Color 
         -> Color 
         -> FPS 
         -> DifficultyLevel 
         -> Maybe FilePath 
         -> IO Env  
buildEnv ts ww wh ph pc bc fps' d mfile = do
  let env = mkEnv ts ww wh ph pc bc fps' d mfile 
  case env of 
    Right env' -> return env'
    Left error' -> fail $ show error'     

-- | Build a 'ArkanoidState' inside IO failing if the input params are wrong.
buildInitialState :: BallPosition
                  -> BallVel
                  -> PlayerPosition
                  -> BrickMap
                  -> Lives
                  -> Env
                  -> IO ArkanoidState 
buildInitialState bp bv p bricks lvs env = do
  let state = mkArkanoidState bp bv p bricks lvs env
  case state of 
    Right state' -> return state'
    Left error'  -> fail $ show error' 

data DifficultyParams = DifficultyParams 
  { bricksPercentage :: Float
  , hitsRange        :: (Int, Int)
  } deriving (Eq, Show)

-- | Build the bricks map.
buildBricks :: WindowWidth -> WindowHeight -> DifficultyParams -> IO BrickMap
buildBricks ww wh dP = let 
    f :: Int -> IO (PositionY, [Brick])  
    f y = do
      bricks <- brickLine ww dP
      return (y, bricks)
    
    n = ceiling $ fromIntegral wh * bricksPercentage dP / 100 

  in M.fromList <$> forM [1 .. n] f

-- | Build a line of bricks.
brickLine :: WindowWidth -> DifficultyParams -> IO [Brick] 
brickLine ww dP = 
  let 
    f :: Int -> IO Brick
    f slot = do
      let minColor = fromEnum (minBound :: Color)
          maxColor = fromEnum (maxBound :: Color)

      size   <- randomRIO (2, slotSize)
      color' <- toEnum <$> randomRIO (minColor, maxColor)
      hits'  <- toEnum <$> randomRIO (hitsRange dP)
      return $ Brick (slot * slotSize) size color' hits'

    slotSize = 5
    slots    = ww `div` slotSize

  in forM [0 .. slots - 1] f
  