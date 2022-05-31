import System.IO

main :: IO ()
main = do
    handleWB <- openFile "woerterbuch.txt" ReadMode
    contentWB <- hGetContents handleWB

    let woerterbuch = lines contentWB

    putStrLn "Geben Sie ein hochdeutsches Wort ein:"
    hDeutsch <- getLine

    let uebersetzung = passendeEintraege hDeutsch woerterbuch
    if null uebersetzung 
        then print hDeutsch 
        else print $ unwords uebersetzung

    hClose handleWB

passendeEintraege :: String -> [String] -> [String]
passendeEintraege _ [] = []
passendeEintraege w (x:xs) | w == head (words x) = last (words x) : passendeEintraege w xs
                           | otherwise = passendeEintraege w xs

passendeEintraege' :: String -> [String] -> [String]
passendeEintraege' w liste = pA w liste []
                             where pA _ [] acc = acc
                                   pA w (x:xs) acc | w == head (words x) = pA w xs (last (words x) : acc)
                                                   | otherwise = pA w xs acc