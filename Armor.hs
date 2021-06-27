module Armor where
import System.IO

data Weapon = Weapon { name :: String, damage :: Double } deriving (Show, Eq)

getWeaponInfo :: Weapon -> String
getWeaponInfo (Weapon name damage) = name ++ " | Daño: " ++ (show damage)

showWeapon :: Weapon -> IO()
showWeapon weapon = do
    putStrLn $ getWeaponInfo(weapon)

getDamage :: Maybe Weapon -> Double
getDamage weapon = case weapon of
                    Nothing -> 0
                    Just (Weapon _ damage) -> damage

getWeaponName :: Weapon -> String
getWeaponName (Weapon name _) = name
