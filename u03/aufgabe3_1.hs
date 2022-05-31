-- aufgabe3_1.hs
import System.IO

main :: IO ()
main = do
    leseGriff <- openFile "palindrom.txt" ReadMode
    inhalt <- hGetContents leseGriff
    ausdrucken inhalt
    hClose leseGriff

ausdrucken :: String -> IO ()
ausdrucken woerter = mapM_ putStrLn (listeErstellen (lines woerter))

listeErstellen :: [String] -> [String]
listeErstellen woerter = [ wort ++ " " ++ show (length wort) ++ " " ++ isPalindrom wort | wort <- woerter ]

isPalindrom :: String -> String
isPalindrom wort = if wort == reverse wort then "ja" else "nein"