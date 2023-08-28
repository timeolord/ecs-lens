{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Monad.State
import Control.Lens
import Data.Default
import Control.Monad
import Data.Map
import Control.Concurrent (threadDelay)

type System m = StateT ECSState m

data ECSState = ECSState {
    _currentEntityID :: Entity,
    _componentsList :: Map Entity Components
} deriving (Show, Eq)

instance Default ECSState where
    def = ECSState def def

newtype Entity = ID Int deriving (Show, Eq, Ord)

instance Num Entity where
    (+) (ID x) (ID y) = ID $ x + y
    (*) (ID x) (ID y) = ID $ x * y
    (-) (ID x) (ID y) = ID $ x - y
    abs (ID x) = ID $ abs x
    signum (ID x) = ID $ signum x
    fromInteger x = ID $ fromInteger x

instance Default Entity where
    def = ID 0

{- User made components -}
data Components = Components {
    _position :: Maybe Position,
    _velocity :: Maybe Velocity,
    _name :: Maybe Name,
    _tag :: Maybe Tag
} deriving (Show, Eq)

instance Default Components where
    def = Components def def def def
newtype Position = Position (Int, Int) deriving (Show, Eq)
newtype Velocity = Velocity (Int, Int) deriving (Show, Eq)
newtype Name = Name String deriving (Show, Eq)
data Tag = Ball | Player deriving (Show, Eq)

gridSize :: (Int, Int)
gridSize = (100, 10)
xSize = fst gridSize
ySize = snd gridSize

makeLenses ''Components
makeLenses ''ECSState

entities :: Fold ECSState Entity
entities = folding $ \s -> view (componentsList . to keys) s

nextEntityID :: Entity -> Entity
nextEntityID (ID x) = ID $ x + 1

spawnEntity :: Components -> System IO Entity
spawnEntity components = do
    x <- gets (^.currentEntityID)
    componentsList %= insert x components
    currentEntityID += 1
    return x

removeEntity :: Entity -> System IO ()
removeEntity entity = componentsList %= delete entity

mainSystem :: System IO ()
mainSystem = do
    let names = ['a'..'z']
    forM_ names $ \name -> do spawnEntity $ Components (Just $ Position (fromEnum name `mod` xSize, (fromEnum 'z' - fromEnum name) `mod` ySize)) (Just $ Velocity ((fromEnum name `mod` 3) + 1, (fromEnum name `mod` 1) + 1)) (Just $ Name [name]) (Just Ball)

    repeatingSystems

clamp :: Ord a => a -> a -> a -> a
clamp min' max' x
    | x < min' = min'
    | x > max' = max'
    | otherwise = x

clampPosition :: (Int, Int) -> Maybe Position
clampPosition (x, y) = Just $ Position (clamp 0 (xSize - 1) x, clamp 0 (ySize - 1) y)

repeatingSystems :: System IO ()
repeatingSystems = do
    movementSystem
    collisionSystem
    renderSystem

    liftIO $ threadDelay 100000
    repeatingSystems

movementSystem :: System IO ()
movementSystem = do
    componentsList %= fmap movement
    where   movement :: Components -> Components
            movement c@(Components {_position = Just (Position (x, y)), _velocity = Just (Velocity (x', y'))})
                 = set position (clampPosition (x + x', y + y')) c
            movement c = c

collisionSystem :: System IO ()
collisionSystem = do
    componentsList %= fmap collision
    where   collision :: Components -> Components
            collision c@(Components {_position = Just (Position (x, y)), _velocity = Just (Velocity (x', y'))})
                | x >= xSize - 1 = set velocity (Just $ Velocity (- x', y')) c
                | x <= 0 = set velocity (Just $ Velocity (- x', y')) c
                | y >= ySize - 1 = set velocity (Just $ Velocity (x', - y')) c
                | y <= 0 = set velocity (Just $ Velocity (x', - y')) c
                | otherwise = c
            collision c = c

renderSystem :: System IO ()
renderSystem = do
    state <- get
    let grid = replicate ySize (replicate xSize ' ')
    let grid' = foldl' renderBalls grid (view componentsList state)
    liftIO $ putStr "\ESC[2J"
    liftIO $ putStrLn $ unlines $ reverse grid'
        where   renderBalls :: [String] -> Components -> [String]
                renderBalls grid c@(Components {_position = Just (Position (x, y)), _velocity = Just (Velocity (x', y')), _tag = Just Ball, _name = Just (Name name)}) = set (element y . element x) (head name) grid

main = void $ runStateT mainSystem def