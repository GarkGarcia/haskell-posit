import Numeric.Posit (Posit32)

posit :: Integer -> Posit32
posit = fromInteger

main :: IO ()
main = do
    let a = 3
    let b = 4

    let c = a + b

    if posit c == posit a + posit b
        then putStrLn "Success!"
        else error "Arithmetic failure"

