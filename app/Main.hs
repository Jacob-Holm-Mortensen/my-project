module Main where

main :: IO String
main = getLine

type K = Integer
type C = Rational
type X = String
data P = CONST C | VAR X | PADD P P | PMUL C P | POW X K deriving (Eq, Show)
data E = POL P | EADD E E | EMUL E E | DIV E E | D X E | SIN E | COS E deriving (Eq, Show)

-- -- Reduction
-- (3)
reduce (EMUL (POL (POW e1 a)) (POL (POW e2 b))) =
            if e1 == e2
            then POL (POW e1 (a+b))
            else EMUL (POL (POW e1 a)) (POL (POW e2 b))
-- (4)
reduce (DIV (POL (POW e1 a)) (POL (POW e2 b))) =
            if e1 == e2
            then POL (POW e1 (a-b))
            else DIV (POL (POW e1 a)) (POL (POW e2 b))
-- (5)
reduce (EADD (EMUL (SIN (POL (VAR e1))) (SIN (POL (VAR e2)))) (EMUL (COS (POL (VAR e3))) (COS (POL (VAR e4))))) =
            if e1 == e2 && e2 == e3 && e3 == e4
            then POL (POW e1 0)
            else EADD (EMUL (SIN (POL (VAR e1))) (SIN (POL (VAR e2)))) (EMUL (COS (POL (VAR e3))) (COS (POL (VAR e4))))

reduce (EADD (EMUL (COS (POL (VAR e1))) (COS (POL (VAR e2)))) (EMUL (SIN (POL (VAR e3))) (SIN (POL (VAR e4))))) =
            if e1 == e2 && e2 == e3 && e3 == e4
            then POL (POW e1 0)
            else EADD (EMUL (COS (POL (VAR e1))) (COS (POL (VAR e2)))) (EMUL (SIN (POL (VAR e3))) (SIN (POL (VAR e4))))
-- (6)
reduce (EADD e1 e2) = EADD (reduce e1) (reduce e2)
-- (7, 9)
reduce (EMUL e1 e2) =
            if e1 == e2 && isVAR e1
            then POL (POW (getVAR e1) 2)
            else EMUL (reduce e1) (reduce e2)
-- (8)
reduce (EADD (EADD e1 e2) e3) = EADD (reduce (EADD e1 e2)) (reduce e3)
-- (10)
reduce (EMUL (EMUL e1 e2) e3)
            | e1 == e2 = EMUL (EMUL e1 e2) e3
            | e2 == e3 = EMUL e1 (EMUL e2 e3)
            | otherwise = EMUL e1 (EMUL e2 e3)
-- (11)
reduce (EMUL e1 (EADD e2 e3)) = EADD (EMUL e1 e2) (EMUL e1 e3)
-- (12)
reduce (POL (PMUL c e)) =
            if c == 1
            then POL e
            else POL (PMUL c e)
-- other reductions
reduce (DIV e1 e2) = DIV (reduce e1) (reduce e2)
-- reduce (D x e1) =
reduce (SIN e1) = SIN (reduce e1)
reduce (COS e1) = COS (reduce e1)
reduce (POL p) = POL (pReduce p)
pReduce (VAR x) = VAR x
pReduce (PMUL c p) = PMUL c (pReduce p)
pReduce (PADD p1 p2) = PADD (pReduce p1) (pReduce p2)
pReduce (POW x k) = POW x k
pReduce (CONST c) = CONST c

-- Helper functions
isVAR (POL (VAR x)) = True
isVAR _ = False

getVAR (POL (VAR x)) = x

-- User functions
t1 = reduce (DIV (EMUL (EMUL (POL (CONST 1))
                             (EADD (EMUL (SIN (POL (VAR "x")))
                                         (SIN (POL (VAR "x"))))
                                   (EMUL (COS (POL (VAR "x")))
                                         (COS (POL (VAR "x"))))))
                       (POL (POW "x" 3)))
                 (POL (POW "x" 2)))