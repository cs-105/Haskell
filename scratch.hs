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

    --Black, Red, Green, Yellow, Blue, Magenta, Cyan, White
    --Vivid, Dull
    --BoldIntensity, FaintIntensity, NormalIntensity


    --Messages
    --Minefield
      --Coordinates
      --Field
        --NewlyRevealed
        --Proximity
        --Flag
        --Unknown
    --Input

    --Conditions:
      --Regular
      --Invalid
      --Game Over
      --Win

--https://hackage.haskell.org/package/ansi-terminal-0.11.1/docs/src/System.Console.ANSI.Codes.html
