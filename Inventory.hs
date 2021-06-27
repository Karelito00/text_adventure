module Inventory where

import System.IO
import Armor

data Inventory = Inventory [Weapon] deriving (Show, Eq)

getItemInventory :: Inventory -> Int -> Weapon
getItemInventory (Inventory (x:xs)) 0 = x
getItemInventory (Inventory (x:xs)) index = getItemInventory (Inventory xs) (index - 1)

showInventory :: Inventory -> Int -> String
showInventory (Inventory []) _ = ""
showInventory (Inventory (x:xs)) count = "[" ++ (show count) ++ "] " ++ getWeaponInfo(x) ++ "\n" ++ showInventory (Inventory xs) (count + 1)

printInventory :: Inventory -> IO ()
printInventory (Inventory inventory) = do
    if length inventory >= 1
    then do
        putStrLn $ "Inventario: "
        putStrLn $ showInventory (Inventory inventory) 1
    else do
        putStrLn $ "Inventario vacío"

