module Arkanoid.Draw 
  ( printer 
  , restoreTerminal
  ) where

import Control.Concurrent.STM ( atomically, readTChan, TChan )
import Control.Lens ( view )
import Control.Monad ( forM_ )
import qualified Data.Map as M
import System.Console.ANSI
    ( Color(Red, White, Green, Magenta),
      clearScreen,
      cursorBackward,
      cursorDown,
      cursorDownLine,
      cursorForward,
      hideCursor,
      setCursorColumn,
      setCursorPosition,
      setSGR,
      showCursor,
      BlinkSpeed(NoBlink, SlowBlink),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      SGR(SetColor, SetBlinkSpeed, Reset) )
import System.IO (hFlush, stdout)

import Arkanoid.DSL
    ( ballPos,
      brickMap,
      lives,
      player,
      score,
      terminated,
      ArkanoidState,
      Env(paddleWidth, windowWidth, windowHeight, paddleColor,
          ballColor) )
import Arkanoid.Types
    ( ArkanoidResult(..),
      Brick(brx, hits, brw),
      BrickMap,
      Hits (..),
      Lives,
      PaddleWidth,
      Score,
      WindowHeight,
      WindowWidth )

type PositionX = Int   
type PositionY = Int
type WallColor = Color

-- | Draw the 'ArkanoidState' snapshots on the terminal.  
-- The snapshots are read from the 'ArkanoidState' channel,  
printer :: Env -> TChan ArkanoidState -> IO ()
printer env chanState = 
  let paddleWidth' = paddleWidth env
      windowWidth'  = windowWidth  env
      windowHeight' = windowHeight env
      paddleColor'  = paddleColor  env
      go = do
          state <- atomically $ readTChan chanState
          let mresult = view terminated state
              (x, y)  = view ballPos state
              p       = view player state
              ballX   = round x
              ballY   = round y
              bricks  = view brickMap state
              score'  = view score state
              lives'  = view lives state 

          resetScreen
          drawTopAndBottomWalls windowWidth' windowHeight' White
          drawBricks bricks
          drawSideWalls windowWidth' windowHeight' White
          drawPaddle p paddleWidth' windowHeight' paddleColor'
          drawScoreAndLives windowHeight' score' lives'

          case mresult of
            Just result -> do
              drawResult (windowWidth' `div` 2 - 4) (windowHeight' `div` 2) result
              hFlush stdout
            Nothing     -> do 
              drawBall ballX ballY (ballColor env)
              hFlush stdout
              go 

  in hideCursor >> go

-- | Draw score and lives.
drawScoreAndLives :: WindowHeight -> Score -> Lives -> IO ()
drawScoreAndLives wh score' lives' = do
  setSGR [Reset]
  setSGR [SetColor Foreground Vivid White]
  setCursorPosition (wh + 2) 0
  putStr $ "SCORE: " <> show score' 
  cursorDownLine 2 
  putStr $ "LIVES: " <> show lives' 

-- | Restore the terminal with the default behavior
-- moving the cursor bellow the last snapshot drawn.
restoreTerminal :: WindowHeight -> IO ()
restoreTerminal windowHeight' = 
  setSGR [Reset] >> showCursor >> setCursorPosition (windowHeight' + 6) 0

-- | Draw the game result. 
drawResult :: PositionX -> PositionY -> ArkanoidResult -> IO ()
drawResult x y result = do
  setSGR [Reset]
  setSGR [SetColor Foreground Vivid Green, SetBlinkSpeed SlowBlink]
  setCursorPosition y x
  let gameOver = "GAME OVER"
  putStr gameOver
  cursorBackward $ length gameOver
  cursorDown 2
  setSGR [SetBlinkSpeed NoBlink]
  case result of 
    Quitted      -> putStr "QUITTED"
    PlayerLose _ -> putStr "GAME LOST"
    PlayerWin  _ -> putStr "GAME WON"  

-- | Reset the screen.
resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- | Draw bricks.
drawBricks :: BrickMap -> IO ()
drawBricks bMap = do
  setSGR [Reset]
  forM_ (M.assocs bMap) $ \(line, bricks) -> do
    setCursorPosition line 0
    forM_ bricks $ \brick -> do
      setCursorColumn $ brx brick
      let color' = case hits brick of 
                      OneH   -> Green 
                      TwoH   -> Magenta
                      ThreeH -> Red

      setSGR [SetColor Foreground Vivid color']
      putStr $ replicate (brw brick) '='

-- | Draw a wall.
drawTopAndBottomWalls :: WindowWidth -> WindowHeight -> WallColor -> IO ()
drawTopAndBottomWalls ww wh c = do
  setSGR [Reset]
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid c]
  putStr $ replicate (ww + 1) '-'
  setCursorPosition wh 0
  putStr $ replicate (ww + 1) '-'

drawSideWalls :: WindowWidth -> WindowHeight -> WallColor -> IO ()  
drawSideWalls ww wh c = do
  setSGR [Reset]
  setSGR [SetColor Foreground Vivid c]
  setCursorPosition 1 0
  forM_ [1 .. wh - 1] $ \_ -> do
    putChar '|'
    cursorForward $ ww -1
    putChar '|'
    cursorDownLine 1

-- | Draw a paddle.
drawPaddle :: PositionX -> PaddleWidth -> WindowHeight -> Color -> IO ()
drawPaddle x pw wh c = do
  let posxB = x + pw - 2
      y     = wh - 1 
  setSGR [Reset] 
  setSGR [SetColor Foreground Vivid c]
  forM_ [x .. posxB] $ \posx -> do 
    setCursorPosition y posx
    putStr "::"

-- Draw the ball.
drawBall :: PositionX -> PositionY -> Color -> IO ()
drawBall x y c = do
  setSGR [Reset]
  setCursorPosition y x
  setSGR [SetColor Foreground Vivid c]
  putStr "O"
