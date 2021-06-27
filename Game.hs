module Game where

import System.IO
import Knight
import Inventory
import Enemies
import Armor
import Utilities
import World

enemyHitKnight :: Enemy -> Knight -> Knight
enemyHitKnight (Enemy _ _ enemyDamage _) (Knight name inventory weapon lifePoints baseDamage) = Knight name inventory weapon (lifePoints - enemyDamage) baseDamage

knightHitEnemy :: Knight -> Enemy -> Enemy
knightHitEnemy (Knight _ _ weapon _ baseDamage) (Enemy name lifePoints damage exp) = Enemy name (lifePoints - (getDamage weapon) - baseDamage) damage exp

fightKnightEnemy :: Knight -> Enemy -> Maybe Knight
fightKnightEnemy knight enemy 
    | knightIsDead newKnight = Nothing
    | enemyIsDead newEnemy = Just newKnight
    | otherwise = fightKnightEnemy newKnight newEnemy
    where
        newKnight = enemyHitKnight enemy knight
        newEnemy = knightHitEnemy knight enemy

game :: World -> Knight -> Int -> IO()
game world knight level = do
    if (level == 8 && (getKnightWeaponName knight) == "Llave de la princesa")
    then do 
        putStrLn $ "Has llegado hasta el final caballero " ++ (getKnightName knight) ++ ", quieres salvar a la princesa o matarla:"
        putStrLn $ "[1] Matar | [2] Salvar"
        option <- getLine
        if isInt option
        then do
            let opt = parseStringToInt option 0
            if(opt == 1)
            then do
                putStrLn $ "Has matado a la princesa y te ha consumido la oscuridad"
                putStrLn $ "---------------------------------------------------------"
            else do
            if (opt == 2) 
            then do 
                putStrLn $ "Genial has salvado a la princesa y hás cumplido con el objetivo del juego"
                putStrLn $ "---------------------------------------------------------"
            else do
                putStrLn $ "Opción desconocida"
                putStrLn $ "---------------------------------------------------------"
                game world knight level
        else do
            putStrLn $ "Opción desconocida"
            putStrLn $ "---------------------------------------------------------"
            game world knight level
        
    else do
        putStrLn $ "---------------------------------------------------------"
        putStrLn $ "Nivel: " ++ (show (level + 1))
        putStrLn $ "Opciones:"
        putStrLn $ "[1] Conocer monstruo | [2] Mis estadísticas | [3] Mostrar inventario | [4] Cambiar arma | [5] Batalla"
        if(level < 8) then putStrLn $ "[6] Ir al siguiente nivel\n" else putStrLn $ "" 
        if(level > 0) then putStrLn $ "[7] Ir al nivel anterior\n" else putStrLn $ "" 

        option <- getLine
        if isInt option
        then do
            let place = getPlaceAtIndex world level
            let opt = parseStringToInt option 0
            let inventory = getInventory knight          

            if (opt == 1) 
            then do 
                putStrLn $ (showEnemyOfPlace place) 
                putStrLn $ "---------------------------------------------------------"
                game world knight level
            else do
            if (opt == 2) 
            then do 
                printKnightInfo knight
                putStrLn $ "---------------------------------------------------------"
                game world knight level
            else do
            if (opt == 3) 
            then do 
                printInventory inventory 
                putStrLn $ "---------------------------------------------------------"
                game world knight level
            else do
            if (opt == 4) 
            then do 
                if((getLengthInventory knight) == 0) 
                then do 
                    printInventory inventory 
                    putStrLn $ "---------------------------------------------------------"
                    game world knight level
                else do
                    putStrLn $ "Elija un arma del inventario"
                    printInventory inventory
                    option <- getLine
                    if isInt option 
                    then do
                        let index = parseStringToInt option 0
                        if (index > 0) && (index <= (getLengthInventory knight))
                        then do
                            let newKnight = changeWeapon knight (getItemInventory inventory (index - 1))
                            putStrLn $ "Arma cambiada exitosamente"
                            putStrLn $ "---------------------------------------------------------"
                            game world newKnight level
                        else do
                            putStrLn $ "Arma no encontrado"
                            putStrLn $ "---------------------------------------------------------"
                            game world knight level
                    else do
                        putStrLn $ "Arma no encontrado"
                        putStrLn $ "---------------------------------------------------------"
                        game world knight level
            else do
            if (opt == 5)
            then do
                let enemy = getEnemyOfPlace place
                putStrLn $ "Comienza la batalla del caballero " ++ (getKnightName knight) ++ " contra " ++ getEnemyName enemy ++ " ..."
                let maybeKnight = fightKnightEnemy knight enemy 
                if (maybeKnight == Nothing) 
                then do 
                    putStrLn "Oh no! Moriste" 
                else do
                    putStrLn $ "Ganaste la batalla!"
                    putStrLn $ "Ganaste más vida y daño, además del arma: " ++ showRewardOfPlace place
                    putStrLn $ "---------------------------------------------------------"
                    let Just newKnight = maybeKnight
                    let rewardKnight = createRewardKnight newKnight place 
                    game world rewardKnight level
            else do
            if level < 8 && opt == 6
            then do
               putStrLn $ "Avanzando al siguiente nivel"
               game world knight (level + 1)
            else do
            if level > 0 && opt == 7
            then do
               putStrLn $ "Regresando al nivel anterior"
               game world knight (level - 1)
            else do
                putStrLn $ "Opción desconocida"
                putStrLn $ "---------------------------------------------------------"
                game world knight level
        else do
            putStrLn $ "Opción desconocida"
            putStrLn $ "---------------------------------------------------------"
            game world knight level


main = do
    putStrLn "Bienvenido a \"Almas y oscuridad\", juego interactivo en el cuál tomarás una serie de decisiones que tendrán consecuencias en el desarrollo de tú personaje y en el desenlace de esta historia.\nTomarás el rol de un caballero que fue encomendado para salvar a la princesa que se encuentra en el último castillo al final de la travesía, tendrás que visitar varios escenarios donde protagonizarás batallas contra monstruos y criaturas que te abrirán paso, posees un inventario con espacio para 10 objetos, y además puedes equiparte con un yelmo, un peto, par de botas y un arma.\nAl comienzo del juego estarás desnudo, solo cuentas con tus manos para atacar. Tienes dos atributos principales: la vida y el daño. Al inicio posees 20 puntos de vida y 5 puntos de daño. Mucha suerte."
    putStrLn "Entra tu alias:"
    name <- getLine
    putStrLn ("Hola, " ++ name ++ ", comienza la aventura!")
    game customWorld (Knight name (Inventory []) Nothing 100 10) 0
