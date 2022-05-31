import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    handleWB <- openFile (args !! 0) ReadMode
    handleOG <- openFile (args !! 1) ReadMode
    contentWB <- hGetContents handleWB
    contentOG <- hGetContents handleOG

    let woerterbuch = lines contentWB
        original = words contentOG

    let uebersetzung = [let passend = passendeEintraege wort woerterbuch in if null passend then wort else head passend | wort <- original]
    print $ unwords uebersetzung

    writeFile "uebersetzung.txt" (unwords uebersetzung)

    hClose handleWB
    hClose handleOG

passendeEintraege :: String -> [String] -> [String]
passendeEintraege _ [] = []
passendeEintraege w (x:xs) | w == head (words x) = last (words x) : passendeEintraege w xs
                           | otherwise = passendeEintraege w xs