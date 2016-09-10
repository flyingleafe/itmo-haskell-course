module Task2 where

import Control.Monad

data Player = Player { attack :: Int, defence :: Int }
data Monster = Vupsen { health :: Int } | Pupsen { health :: Int }
data Result = Victory | Defeat deriving Show
data Equip = Shield | Sword

monsterAttack :: Monster -> Int
monsterAttack (Vupsen _) = 5
monsterAttack (Pupsen _) = 2

monsterLoot :: Monster -> Equip
monsterLoot (Vupsen _) = Sword
monsterLoot (Pupsen _) = Shield

applyEquip :: Equip -> Player -> Player
applyEquip Shield pl = pl { defence = defence pl + 5 }
applyEquip Sword pl = pl { attack = attack pl + 3 }

fightMonster' :: Bool -> Player -> Monster -> Maybe Player
fightMonster' playerFirst player monster
  | defence player <= 0 = Nothing
  | health monster <= 0 = Just $ applyEquip (monsterLoot monster) player
  | playerFirst = fightMonster' False player monster { health = health monster - attack player }
  | otherwise = fightMonster' True player { defence = defence player - monsterAttack monster } monster

fightMonster :: Player -> Monster -> Maybe Player
fightMonster = fightMonster' True

gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle pl enemies = case foldM fightMonster pl enemies of
                              Just _ -> Victory
                              Nothing -> Defeat

defaultPlayer = Player 3 5
defaultVupsen = Vupsen 4
defaultPupsen = Pupsen 6

round1 = [defaultPupsen]
round2 = [defaultPupsen, defaultPupsen]
round3 = [defaultPupsen, defaultVupsen]
impossibru = [defaultVupsen]
