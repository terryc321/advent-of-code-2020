
-- > idris2
-- > :l "fun.idr"
-- > :c fun main       compile file fun with entry point main i guess
--  generates build/exec/fun written 
-- > idris2 fun.idr
-- > :t isAllowed      shows type of isAllowed function
-- Main.isAllowed : Char -> Bool
--
-- how can we generate interactive book ?
-- interactive documentation

module Main

two : Int
two = 2

-- > Main.pi 
-- 3.14
pi : Double
pi = 3.14 

-- character
zed : Char
zed = 'z'

-- equality over character
zedIsZ = 'z' == zed



-- Allowed characters
isAllowed : Char -> Bool
isAllowed c = c == '0' || c == '1' || c == 'X'
-- > isAllowed '0'
-- True



-- -- A type-level predicate: all characters must be allowed
-- AllAllowed : Vect n Char -> Type
-- AllAllowed [] = ()
-- AllAllowed (x :: xs) = (isAllowed x = True, AllAllowed xs)

-- -- The main type: 36 characters, all allowed
-- AllowedMask : Type
-- AllowedMask = (xs : Vect 36 Char ** AllAllowed xs)


main : IO ()
main = putStrLn "Hello world"

