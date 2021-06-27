module World where

import Enemies
import Armor

data Place = Place { enemy :: Enemy, reward :: Weapon } deriving Show
data World = World [Place]

getPlaceAtIndex :: World -> Int -> Place
getPlaceAtIndex (World (x:xs)) 0 = x
getPlaceAtIndex (World (x:xs)) index = getPlaceAtIndex (World xs) (index - 1)

getEnemyOfPlace :: Place -> Enemy
getEnemyOfPlace (Place enemy _) = enemy

showEnemyOfPlace :: Place -> String
showEnemyOfPlace (Place enemy _) = showEnemy enemy

showRewardOfPlace :: Place -> String
showRewardOfPlace (Place _ reward) = getWeaponInfo reward

level_1 = Place centauro (Weapon "Garras" 5)
level_2 = Place zombies (Weapon "Pala" 7)
level_3 = Place dark_knight (Weapon "Espada oscura" 9)
level_4 = Place skeleton (Weapon "Espada larga" 8)
level_5 = Place abyss_watcher (Weapon "Espada flameante" 10)
level_6 = Place dark_prince (Weapon "Espada del príncipe oscuro" 12)
level_7 = Place zeus (Weapon "Relámpago" 10)
level_8 = Place albright (Weapon "Mazo de hierro" 13)
level_9 = Place princess_watcher (Weapon "Llave de la princesa" 0)

customWorld = World [level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8, level_9]
