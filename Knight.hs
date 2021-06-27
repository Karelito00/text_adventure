module Knight where

import Inventory
import Armor
import World
import Enemies

data Knight = Knight { name :: String, inventario :: Inventory, weapon :: Maybe Weapon, lifePoints :: Double, baseDamage :: Double} deriving (Show, Eq)

getKnightWeaponName :: Knight -> String
getKnightWeaponName (Knight _ _ weapon _ _) = case weapon of
                                                Nothing -> "Nada"
                                                Just weapon -> getWeaponName weapon
                                                
createRewardKnight :: Knight -> Place -> Knight
createRewardKnight (Knight name (Inventory inventory) weapon lifePoints baseDamage) (Place (Enemy _ _ _ exp) weaponReward) = Knight name (Inventory (weaponReward:inventory)) weapon (100 + (0.40 * exp)) (baseDamage + (0.08 * exp))

getKnightName :: Knight -> String
getKnightName (Knight name _ _ _ _) = name

knightIsDead :: Knight -> Bool
knightIsDead (Knight _ _ _ lifePoints _)
    | lifePoints > 0 = False
    | otherwise = True

showKnightInventory :: Knight -> IO ()
showKnightInventory (Knight _ inventory _ _ _) = do
    printInventory inventory

changeWeapon :: Knight -> Weapon -> Knight
changeWeapon (Knight name inventory _ lifePoints baseDamage) newWeapon = Knight name inventory (Just newWeapon) lifePoints baseDamage

getLengthInventory :: Knight -> Int
getLengthInventory (Knight _ (Inventory inventory) _ _ _) = length inventory

getInventory :: Knight -> Inventory
getInventory (Knight _ inventory _ _ _) = inventory

printKnightInfo :: Knight -> IO ()
printKnightInfo (Knight knightName inventory weapon lifePoints baseDamage) = do
    putStrLn $ "Nombre: " ++ knightName ++ " | Puntos de Vida: " ++ (show lifePoints) ++ " | Daño: " ++ (show baseDamage) ++ " + " ++ (show (getDamage(weapon)))
    putStrLn $ ""

    if weapon == Nothing
    then do
        putStrLn $ "Arma equipada: manos."
    else do
        let Just weaponUsed = weapon
        putStrLn $ "Arma equipada: "
        showWeapon(weaponUsed)

    putStrLn $ ""

    printInventory(inventory)

