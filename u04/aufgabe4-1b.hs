import System.IO

main :: IO ()
main = do
    handleWB <- openFile "woerterbuch.txt" ReadMode
    handleOG <- openFile "original.txt" ReadMode
    contentWB <- hGetContents handleWB
    contentOG <- hGetContents handleOG

    let woerterbuch = lines contentWB
        original = words contentOG

    let uebersetzung = [let passend = passendeEintraege wort woerterbuch in if null passend then wort else head passend | wort <- original]
    print $ unwords uebersetzung

    hClose handleWB
    hClose handleOG

passendeEintraege :: String -> [String] -> [String]
passendeEintraege _ [] = []
passendeEintraege w (x:xs) | w == head (words x) = last (words x) : passendeEintraege w xs
                           | otherwise = passendeEintraege w xs