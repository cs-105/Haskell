import System.Console.ANSI
--System.Console.ANSI >=0.8.0.1 && <0.8.0.2

--only implement once the imports have been figured out

main :: IO ()
main=sgrExample

sgrExample :: IO ()
sgrExample = do
    setSGR [SetColor Foreground Vivid Red]
    setSGR [SetColor Background Vivid Blue]
    putStr "Red-On-Blue"
    setSGR [Reset]
    putStr "White-On-Black"
