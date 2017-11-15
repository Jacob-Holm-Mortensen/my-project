module Main where

main :: IO String
main = getLine

type K = Integer
type C = Rational
type X = String
data P = VAR X | PADD P P | PMUL C P | POW X K deriving (Eq, Show)
data E = POL P | EADD E E | EMUL E E | DIV E E | D X E | SIN E | COS E deriving (Eq, Show)

-- -- Reduction
startReducing e =
            if e == reduce e
            then e
            else startReducing (reduce e)
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

reduce (EADD e1 (EADD e2 e3)) = EADD (reduce e1) (reduce (EADD e2 e3))
-- (10)
reduce (EMUL (EMUL e1 e2) e3) = EMUL (reduce (EMUL e1 e2)) (reduce e3)

reduce (EMUL e1 (EMUL e2 e3)) = EMUL (reduce e1) (reduce (EMUL e2 e3))
-- (11)
reduce (EMUL (EADD e1 e2) e3) = EADD (EMUL e1 e3) (EMUL e1 e3)

reduce (EMUL e1 (EADD e2 e3)) = EADD (EMUL e1 e2) (EMUL e1 e3)
-- (12)
reduce (EMUL e (POL (POW x 0))) = e

reduce (EMUL (POL (POW x 0)) e) = e
-- other reductions
reduce (DIV e1 e2) = DIV (reduce e1) (reduce e2)
reduce (D x e)
            | isCOS e = neg (SIN (getCOS e))
            | isSIN e = COS (getSIN e)
            | isNEG e && isCOS (getNEG e) = SIN (getCOS (getNEG e))
            | isNEG e && isSIN (getNEG e) = neg (COS (getSIN (getNEG e)))
            | isPOW e = POL (PMUL (fromInteger (getPOWk e)) (POW (getPOWx e) ((getPOWk e) - 1)))
            | isPMUL e =
            | isEMUL e = EMUL (reduce (D x (getEMULe1 e))) (reduce (D x (getEMULe2 e)))
            | isPADD e =
            | isEADD e = EADD (reduce (D x (getEADDe1 e))) (reduce (D x (getEADDe2 e)))
reduce (SIN e) = SIN (reduce e)
reduce (COS e) = COS (reduce e)
reduce (POL p) = POL (pReduce p)
pReduce (VAR x) = VAR x
pReduce (PMUL c p) =
            if c == 1
            then p
            else PMUL c (pReduce p)
pReduce (PADD p1 p2) = PADD (pReduce p1) (pReduce p2)
pReduce (POW x k) =
            if k == 1
            then VAR x
            else POW x k

-- Helper functions
isVAR (POL (VAR x)) = True
isVAR _ = False
getVAR (POL (VAR x)) = x

isCOS (COS e) = True
isCOS _ = False
getCOS (COS e) = e

isSIN (SIN e) = True
isSIN _ = False
getSIN (SIN e) = e

isPOW (POL (POW x k)) = True
isPOW _ = False
getPOWx (POL (POW x k)) = x
getPOWk (POL (POW x k)) = k

isNEG (EMUL (POL (PMUL c (POW x 0))) e) =
            if c < 0
            then True
            else False
isNEG _ = False
getNEG (EMUL (POL (PMUL c (POW x 0))) e) = e

neg e = EMUL (POL (PMUL (-1) (POW "x" 0))) e
-- User functions
t1 = startReducing (DIV (EMUL (EMUL (POL  (POW "x" 0))
                                    (EADD (EMUL (SIN (POL (VAR "x")))
                                                (SIN (POL (VAR "x"))))
                                          (EMUL (COS (POL (VAR "x")))
                                                (COS (POL (VAR "x"))))))
                              (POL  (POW "x" 3)))
                        (POL  (POW "x" 2)))