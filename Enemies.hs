module Enemies where

data Enemy = Enemy { enemyName :: String, lifePoints :: Double, damage :: Double, exp :: Double } deriving Show

enemyIsDead :: Enemy -> Bool
enemyIsDead (Enemy _ lifePoints _ _)
    | lifePoints > 0 = False
    | otherwise = True

showEnemy :: Enemy -> String
showEnemy (Enemy name lifePoints damage exp) = "Nombre: " ++ name ++ " | Puntos de vida: " ++ (show lifePoints) ++ " | Daño: " ++ (show damage) ++ " | Experiencia: " ++ (show exp)

getEnemyName :: Enemy -> String
getEnemyName (Enemy name _ _ _) = name

-- Easy enemies
centauro = Enemy "Centauro" 25 10 12
dark_knight = Enemy "Caballero Oscuro" 30 15 15
zombies = Enemy "Grupo de zombies" 38 12 14 

-- Medium enemies
dark_prince = Enemy "Príncipe oscuro" 50 18 20
abyss_watcher = Enemy "Vigilante del abismo" 48 17 18
skeleton = Enemy "Esqueleto" 48 16 17

-- Hard enmies
princess_watcher = Enemy "Vigilante de la princesa" 100 25 30
zeus = Enemy "Zeus" 95 20 24
albright = Enemy "Caballero Albright" 95 23 30

