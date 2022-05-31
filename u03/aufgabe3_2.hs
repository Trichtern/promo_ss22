-- aufgabe3_2.hs
import System.IO

main :: IO ()
main = do
    putStrLn "Deutsches Wort:"
    deutsch <- getLine
    putStrLn "Bairisches Wort:"
    bairisch <- getLine

    if null deutsch || null bairisch -- beendet die Schleife
        then return ()
        else do
            putStrLn ("'" ++ deutsch ++ "' heisst auf Bairisch '" ++ bairisch ++ "'")
            schreibGriff <- openFile "woerterbuch.txt" AppendMode
            hPutStr schreibGriff (deutsch ++ " " ++ bairisch ++ "\n")
            hClose schreibGriff
            main -- rekursiver Aufruf fuer die Schleife
