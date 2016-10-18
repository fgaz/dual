{-# LANGUAGE DeriveGeneric #-}

import Network.WebSockets hiding (Message)
import Network.Socket (withSocketsDo)
import Graphics.Gloss.Interface.IO.Game
import Data.Bifunctor
import Data.Biapplicative
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.List (partition)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (forever)
import GHC.Generics
import Data.Serialize (Serialize, encodeLazy, decodeLazy)
import Control.Monad
import System.Environment
import System.Exit


type Time = Float
type Hp = Float
type Position = (Float, Float)
type Velocity = (Float, Float)
type BSize = Float
type OtherLost = Bool

data Bullet = Bullet BSize Position Velocity deriving (Show, Read, Generic)

data GameState = Playing | Won | Lost | Even deriving (Show, Eq, Read, Generic)

data Ammo = Ammo { availAmmo :: Float
                 , charging :: Bool
                 , charged :: Float }
                 deriving (Show, Read, Generic)

data World = World { pos :: Position
                   , hp :: Hp
                   , ammo :: Ammo
                   , velocity :: Velocity
                   , gameState :: GameState
                   , ownBullets :: [Bullet]
                   , othBullets :: [Bullet] }
                   deriving (Show, Read, Generic)

data Role = Server Int | Client String Int deriving (Show, Read)

data Message = ILost | IShot Bullet deriving (Show, Read, Generic)

instance Serialize Bullet
instance Serialize GameState
instance Serialize Ammo
instance Serialize World
instance Serialize Message

fromRight (Right a) = a --FIXME ignore, report or something
fromRight _ = error "deserialization error. sorry :("

instance WebSocketsData Message where
  fromLazyByteString = fromRight . decodeLazy
  toLazyByteString = encodeLazy

instance WebSocketsData Bullet where
  fromLazyByteString = fromRight . decodeLazy
  toLazyByteString = encodeLazy

maxAmmo = 6
chargingSpeed = 2
rechargingSpeed = 2
minCharge = 1.0
addedBulletVelocity = 2
bulletVelocity = 200
playerVelocity = 200
maxHp = 100
damageMultiplier = 10
playerHitBox = (playerWidth/2, playerHeight/2) -- half the size of the hitbox side
playerWidth = 80
playerHeight = 80

screenW = 800
screenH = 600

ownColor = magenta
othColor = yellow
lifeColor = green
ammoColor = red

fps = 60

main :: IO ()
main = do
  args <- getArgs
  role <- case args of ip:port:[] -> return $ Client ip (read port)
                       port:[] -> return $ Server $ read port
                       _ -> do
                               putStrLn "Invalid arguments."
                               putStrLn "Usage:"
                               putStrLn "  dual address port      - connects to an existing server"
                               putStrLn "  dual port              - creates a server"
                               exitFailure
  sendChan <- newChan
  recvMVar <- newMVar []
  otherLostMVar <- newMVar False
  _ <- forkIO $ case role of
    Server port -> do
      runServer "0.0.0.0" port $ ws otherLostMVar recvMVar sendChan <=< acceptRequest
    Client ip port -> do
      withSocketsDo $ runClient ip port "/" $ ws otherLostMVar recvMVar sendChan
  playIO (InWindow "DUAL" (screenW, screenH) (0, 0))
    black
    fps
    defaultWorld
    render
    handleInput
    (step otherLostMVar recvMVar sendChan)
  --TODO clean up the socket properly

ws :: MVar OtherLost -> MVar [Bullet] -> Chan Message -> Connection -> IO ()
ws gameStateM rM sC conn = do
  _ <- forkIO $ recvData conn gameStateM rM
  sendData conn sC

sendData :: Connection -> Chan Message -> IO ()
sendData conn ch = do
  forever $ do
    m <- readChan ch
    sendBinaryData conn m
  --hClose hdl

recvData :: Connection -> MVar OtherLost -> MVar [Bullet] -> IO ()
recvData conn gsMV bMV = do
  forever $ do
    msg <- receiveData conn
    case msg of IShot b -> modifyMVar_ bMV  (\bs -> return (b:bs))
                ILost   -> modifyMVar_ gsMV (return . const True)
  --hClose hdl

defaultAmmo :: Ammo
defaultAmmo = Ammo { availAmmo = maxAmmo
                   , charging = False
                   , charged = 0 }

defaultWorld :: World
defaultWorld = World { pos = (0,0)
                     , hp = maxHp
                     , ammo = defaultAmmo
                     , velocity = (0,0) 
                     , gameState = Playing
                     , ownBullets = []
                     , othBullets = [] }

render :: Monad m => World -> m Picture
render world = return $ renderBullets world
                      <> statePicture (gameState world)
                      <> borders
                      <> translate x y (color ownColor $ rectangleSolid playerWidth playerHeight)
                      <> translate x (y - playerHeight / 2 - 5) (color ammoColor $ rectangleSolid (10* availAmmo (ammo world)) 5)
                      <> if hp world > 0
                           then translate x (y - playerHeight / 2 - 13) $ color lifeColor $ rectangleSolid (1* hp world) 5
                           else Blank
  where x = fst $ pos world
        y = snd $ pos world

statePicture :: GameState -> Picture
statePicture Won  = Translate (-350) 0 $ Color red $ Graphics.Gloss.Interface.IO.Game.Text "YOU WON"
statePicture Lost = Translate (-350) 0 $ Color red $ Graphics.Gloss.Interface.IO.Game.Text "YOU LOST"
statePicture Even = Translate (-350) 0 $ Color red $ Graphics.Gloss.Interface.IO.Game.Text "EVEN"
statePicture Playing = Blank

borders :: Picture
borders = color white $ rectangleWire (fromIntegral screenW - 2) (fromIntegral screenH - 2)

renderBullets :: World -> Picture
renderBullets world = Pictures (fmap (renderBullet ownColor) (ownBullets world) ++ fmap (renderBullet othColor) (othBullets world))

renderBullet :: Color -> Bullet -> Picture
renderBullet c (Bullet size (x,y) _) = translate x y $ color c $ rectangleSolid (10*size) (10*size)

handleInput :: Monad m => Event -> World -> m World
handleInput (EventKey (SpecialKey KeyDown)  Down _ _) world = return world { velocity = second (const (-1)) $ velocity world }
handleInput (EventKey (SpecialKey KeyDown)  Up   _ _) world = return world { velocity = second (const    0) $ velocity world }
handleInput (EventKey (SpecialKey KeyUp)    Down _ _) world = return world { velocity = second (const    1) $ velocity world }
handleInput (EventKey (SpecialKey KeyUp)    Up   _ _) world = return world { velocity = second (const    0) $ velocity world }
handleInput (EventKey (SpecialKey KeyLeft)  Down _ _) world = return world { velocity = first  (const (-1)) $ velocity world }
handleInput (EventKey (SpecialKey KeyLeft)  Up   _ _) world = return world { velocity = first  (const    0) $ velocity world }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) world = return world { velocity = first  (const    1) $ velocity world }
handleInput (EventKey (SpecialKey KeyRight) Up   _ _) world = return world { velocity = first  (const    0) $ velocity world }
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world = return world { ammo = (ammo world) { charging = True } }
handleInput (EventKey (SpecialKey KeySpace) Up   _ _) world = return world { ammo = (ammo world) { charging = False } }
handleInput _ world = return world

-- Just a bottom-to-top chain of world modifications.
-- The effectful ones are marked with =<<, which is nice.
step :: MVar OtherLost -> MVar [Bullet] -> Chan Message -> Time -> World -> IO World
step stateMVar rCh sCh dt world =
  lose sCh =<<
  sendBullets sCh =<<
  ((destroyOthBullets .
  getHit .
  moveBullets dt .
  movePlayer dt .
  shoot .
  charge dt .
  recharge dt) <$>
  (receiveBullets rCh =<<
  receiveVictory stateMVar world))

receiveBullets :: MVar [Bullet] -> World -> IO World
receiveBullets mv world = do
  bs <- swapMVar mv [] --XXX FIXME not atomic. -> STM/synchronous-channels?
  let bs' = flipVelocity <$> bs
  return world { othBullets = othBullets world ++ bs' }

receiveVictory :: MVar OtherLost -> World -> IO World
receiveVictory otherLostMVar world = do
  won' <- tryReadMVar otherLostMVar --TODO avoid if already won? or laziness already does it?
  let won = case won' of Just a -> a
                         Nothing -> error "Logic error: the game state MVar should always be full!" --TODO replace False with emptyness? use a lock?
  return $ if not won then world
           else world { gameState = f $ gameState world }
             where f Playing = Won
                   f Lost = Even
                   f Even = Even
                   f Won = Won

flipVelocity :: Bullet -> Bullet
flipVelocity (Bullet s p (vx, vy)) = Bullet s p (vx, -vy)

sendBullets :: Chan Message -> World -> IO World
sendBullets ch world = do
  let (onScreen, offScreen) = partition (\(Bullet _ (_, y) _) -> y<=fromIntegral screenH/2) $ ownBullets world
  let reallyOnScreen = filter (\(Bullet _ (x, _) _) -> x<=fromIntegral screenW/2 && x>=fromIntegral screenW/(-2)) onScreen
  writeList2Chan ch $ fmap IShot offScreen
  return world { ownBullets = reallyOnScreen }

lose :: Chan Message -> World -> IO World
lose ch world | gameState world /= Playing = return world
              | hp world <= 0 = do
                                   writeChan ch ILost
                                   return $ world { gameState = Lost }
              | otherwise = return world

getHit :: World -> World
getHit world = world'
  where world' = world { hp = hp' }
        bs = othBullets world
        hp' = hp world - sum (catMaybes $ bulletDamage world <$> bs)

bulletDamage :: World -> Bullet -> Maybe Hp
bulletDamage world (Bullet s p _) = if inHitBox playerHitBox (pos world) p
                                      then Just (s*damageMultiplier)
                                      else Nothing

-- | if the two positions are within the given range
inHitBox :: (Float, Float) -> Position -> Position -> Bool
inHitBox (hx, hy) (ax, ay) (bx, by) = x && y
  where x = abs (ax-bx) < hx
        y = abs (ay-by) < hy

destroyOthBullets :: World -> World
destroyOthBullets world = world { othBullets = filter (\(Bullet _ p _) -> not (hit p || outside p)) $ othBullets world }
  where hit p = inHitBox playerHitBox p $ pos world
        outside (x,y) = x >  (fromIntegral screenW)/2
                     || x < -(fromIntegral screenW)/2
                     || y < -(fromIntegral screenH)/2
--TODO add 'life' property to Bullet
--TODO or delete off-screen

movePlayer :: Time -> World -> World
movePlayer dt world = world { pos = checkBounds $ bimap (+) (+) (bimap (*(playerVelocity*dt)) (*(playerVelocity*dt)) $ velocity world) <<*>> pos world }

checkBounds :: Position -> Position
checkBounds (x,y) = (max (-halfW) $ min halfW x, max (-halfH) $ min halfH y)
  where halfW = fromIntegral screenW / 2
        halfH = fromIntegral screenH / 2

recharge :: Time -> World -> World
recharge dt world = world { ammo = a' }
  where a' = if charging a
               then a
               else a { availAmmo = min maxAmmo (availAmmo a + dt*rechargingSpeed) }
        a = ammo world

charge :: Time -> World -> World
charge dt world = world { ammo = a' }
  where a = ammo world
        a' = a { availAmmo = availAmmo', charged = charged' }
        availAmmo' = if charging a
                       then max 0 $ availAmmo a - chargeAmount
                       else availAmmo a
        charged' = if charging a && availAmmo a - chargeAmount > 0
                     then min maxAmmo $ charged a + chargeAmount
                     else charged a
        chargeAmount = if charged a > 0
                         then dt*chargingSpeed
                         else minCharge

shoot :: World -> World
shoot world = world { ammo = a', ownBullets = bs' }
  where a = ammo world
        bs = ownBullets world
        a' = if shooting then a { charged = 0 } else a
        bs' = if shooting then b:bs else bs
        b = mkBullet world
        shooting = not (charging a) && charged a > 0 && hp world > 0

mkBullet :: World -> Bullet
mkBullet world = Bullet s p v
  where s = charged $ ammo world
        p = pos world
        v = second (+addedBulletVelocity* sqrt s) $ velocity world

moveBullets :: Time -> World -> World
moveBullets dt world = world { ownBullets = moveBullet dt <$> ownBullets world
                             , othBullets = moveBullet dt <$> othBullets world }

moveBullet :: Time -> Bullet -> Bullet
moveBullet dt (Bullet s p v) = Bullet s (bimap (+) (+) (bimap (*(bulletVelocity*dt)) (*(bulletVelocity*dt)) v) <<*>> p) v
