{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk (runSuccessProb) where

import Control.Monad.Random
import Control.Monad.Writer
import Data.List(sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

-- Define that application monad stack a monad access functions. This type and related functions will allow us to
-- change the stack of monad transformers without breaking existing code.
type AppMonadStack = (WriterT String (Rand StdGen))

-- Access the Rand monad underneath the WriterT monad transformer in (WriterT String (Rand StdGen)) a
accessRand a = lift a

-- Access the Writer monad in (WriterT String (Rand StdGen)) a
accessWriter a = a

runSuccessProb :: Int -> Int -> Int -> IO (Double, String)
runSuccessProb iterationCount aCount dCount = do
    gen <- newStdGen
    let (prob, debugOutput) = evalRand (runWriterT (successProb iterationCount (Battlefield aCount dCount))) gen
    return (prob, debugOutput)

successProb :: Int -> Battlefield -> AppMonadStack Double
successProb iterationCount bf = liftM (\result -> result / (fromIntegral iterationCount)) (foldM foldFunc 0 [1..iterationCount])
  where
    foldFunc = (\acc _ -> (liftM (\resultBf -> (if defenders resultBf == 0 then acc+1 else acc)) (invade bf)))

invade :: Battlefield -> AppMonadStack Battlefield
invade bf@(Battlefield _ 0) = return bf
invade bf@(Battlefield 1 _) = return bf
invade bf = do
            bf' <- battle bf
            invade bf'

battle :: Battlefield -> AppMonadStack Battlefield
battle bf = do
              ar1 <- accessRand die
              ar2 <- accessRand die
              ar3 <- accessRand die
              dr1 <- accessRand die
              dr2 <- accessRand die
              accessWriter tell("Attacker(" ++ show (attackers bf) ++ "): " ++ show (unDV ar1)++"," ++ show (unDV ar2) ++ "," ++ show (unDV ar3) ++
                " Defender(" ++ show (defenders bf) ++ "): " ++ show (unDV dr1) ++ "," ++ show (unDV dr2) ++ "\n")
              return (recalcBattlefield (ar1 : ar2 : ar3 : []) (dr1 : dr2 : []) bf)

-- Given a set of attacker and defender die rolls and a Battlefield, apply the die rolls
-- according to the rules for risk and return a modified Battlefield
recalcBattlefield :: [DieValue] -> [DieValue] -> Battlefield -> Battlefield
recalcBattlefield arlist drlist (Battlefield acount dcount) = Battlefield (acount - aloss) (dcount - dloss)
  where
    -- attacking player may attack with up to three units at a time.
    -- However, they must always leave at least one unit behind.
    attackDiceCnt = min 3 (acount-1)
    -- defending player may defend with up to two units (or only one if that is all they have).
    defenceDiceCnt = min 2 (dcount)

    arlist' = sortBy (flip compare) $ take attackDiceCnt arlist -- the attacker dice roll list
    drlist' = sortBy (flip compare) $ take defenceDiceCnt drlist -- the defender dice roll list

    engagementResult = engage arlist' drlist' (0,0)

    aloss = fst engagementResult -- attacker losses
    dloss = snd engagementResult -- defender losses

-- Using provided die rolls, calculate the number attacker and defender losses.
-- Engage calls itself until no more attacker or defender die rolls are available.
engage :: [DieValue] -> [DieValue] -> (Int, Int) -> (Int, Int)
engage [] _ lossCount = lossCount
engage _ [] lossCount = lossCount
engage (ar:ars) (dr:drs) (aloss, dloss) =
  case (compare ar dr) of
    GT -> engage ars drs (aloss, dloss+1)
    otherwise -> engage ars drs (aloss+1, dloss)