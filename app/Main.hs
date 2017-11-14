-- module Main where

-- main :: IO String
-- main = getLine

one = 1

type K = Integer
type C = Rational
type X = String
data P = CONST C | VAR X | PADD P P | PMUL C P | PPOW X K deriving (Eq, Show)
data E = POL P | EADD E E | EMUL E E | DIV E E | D E | SIN E | COS E | EPOW E K deriving (Eq, Show)

-- -- Reduction
-- (3)
reduce (EMUL (POL (PPOW e1 a)) (POL (PPOW e2 b))) = if e1 == e2
                                                  then POL (PPOW e1 (a+b))
                                                  else EMUL (POL (PPOW e1 a)) (POL (PPOW e2 b))

-- (4)
reduce (DIV (POL (PPOW e1 a)) (POL (PPOW e2 b))) = if e1 == e2
                                                 then POL (PPOW e1 (a-b))
                                                 else DIV (POL (PPOW e1 a)) (POL (PPOW e2 b))

-- (5)
reduce (EADD (EMUL (SIN (POL (VAR e1))) (SIN (POL (VAR e2)))) (EMUL (COS (POL (VAR e3))) (COS (POL (VAR e4))))) = if e1 == e2 && e2 == e3 && e3 == e4
                                                                                                                  then POL (PPOW e1 0)
                                                                                                                  else EADD (EMUL (SIN (POL (VAR e1))) (SIN (POL (VAR e2)))) (EMUL (COS (POL (VAR e3))) (COS (POL (VAR e4))))

-- (6)
reduce (EADD e1 e2) = EADD (reduce e2) (reduce e1)

-- (7, 9)
-- reduce (EMUL e1 e2) = let a = reduce e1
--                           b = reduce e2
--                       in if a == b && isPOL a
--                          then POL (getPOL a)
--                          else EMUL b a

-- (8)
reduce (EADD (EADD e1 e2) e3) = EADD e1 (EADD e2 e3)

-- (10)
reduce (EMUL (EMUL e1 e2) e3) | reduce e1 == reduce e2 = EMUL (reduce (EMUL (reduce e1) (reduce e2))) (reduce e3) -- && isPOL a
                              | reduce e2 == reduce e3 = EMUL (reduce e1) (reduce (EMUL (reduce e2) (reduce e3)))
                              | otherwise = EMUL (reduce e1) (EMUL (reduce e2) (reduce e3))

-- (11)
reduce (EMUL e1 (EADD e2 e3)) = let a = reduce e1
                                    b = reduce e2
                                    c = reduce e3
                                in EADD (EMUL a b) (EMUL a c)

-- (12)
reduce (EMUL e1 e2) | e1 == POL (CONST one) = reduce e2
                    | e2 == POL (CONST one) = reduce e1
                    | otherwise = EMUL (reduce e1) (reduce e2)

-- Helpers
-- getPOL (POL p1) = p1

-- isPOL (POL p1) = True
-- isPOL _ = False

add (VAR e1) (VAR e2) = PADD (VAR e1) (VAR e2)
mul = "not implemented"
mulScal = "not implemented"
div = "not implemented"
diff = "not implemented"