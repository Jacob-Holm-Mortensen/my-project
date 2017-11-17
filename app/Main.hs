module Main where

main :: IO String
main = getLine

type K = Integer
type C = Rational
type X = String
data P = VAR X | PADD P P | PMUL C P | PPOW X K deriving (Eq, Show)
data E = POL P | EADD E E | EMUL E E | EPOW E K | DIV E E | D X E | SIN E | COS E deriving (Eq, Show)

-- -- Reduction
-- initiate reduction
startReducing e =
            if e == reduce e
            then e
            else startReducing (reduce e)
-- (3)
reduce (EMUL (POL (PPOW e1 a)) (POL (PPOW e2 b))) =
            if e1 == e2
            then POL (PPOW e1 (a+b))
            else EMUL (POL (PPOW e1 a)) (POL (PPOW e2 b))

reduce (EMUL (EPOW e1 a) (EPOW e2 b)) =
            if reduce e1 == reduce e2
            then EPOW (reduce e1) (a+b)
            else EMUL (EPOW (reduce e1) a) (EPOW (reduce e2) b)
-- (4)
reduce (DIV (POL (PPOW e1 a)) (POL (PPOW e2 b))) =
            if e1 == e2
            then POL (PPOW e1 (a-b))
            else DIV (POL (PPOW e1 a)) (POL (PPOW e2 b))

reduce (DIV (EPOW e1 a) (EPOW e2 b)) =
            if reduce e1 == reduce e2
            then EPOW (reduce e1) (a-b)
            else DIV (EPOW (reduce e1) a) (EPOW (reduce e2) b)
-- (5)
reduce (EADD (EMUL (SIN (POL (VAR e1))) (SIN (POL (VAR e2)))) (EMUL (COS (POL (VAR e3))) (COS (POL (VAR e4))))) =
            if e1 == e2 && e2 == e3 && e3 == e4
            then one
            else EADD (EMUL (SIN (POL (VAR e1))) (SIN (POL (VAR e2)))) (EMUL (COS (POL (VAR e3))) (COS (POL (VAR e4))))

reduce (EADD (EMUL (COS (POL (VAR e1))) (COS (POL (VAR e2)))) (EMUL (SIN (POL (VAR e3))) (SIN (POL (VAR e4))))) =
            if e1 == e2 && e2 == e3 && e3 == e4
            then one
            else EADD (EMUL (COS (POL (VAR e1))) (COS (POL (VAR e2)))) (EMUL (SIN (POL (VAR e3))) (SIN (POL (VAR e4))))

reduce (EADD (EPOW (SIN e1) 2) (EPOW (COS e2) 2)) =
            if reduce e1 == reduce e2
            then one
            else EADD (EPOW (SIN (reduce e1)) 2) (EPOW (COS (reduce e2)) 2)

reduce (EADD (EPOW (COS e1) 2) (EPOW (SIN e2) 2)) =
            if reduce e1 == reduce e2
            then one
            else EADD (EPOW (COS (reduce e1)) 2) (EPOW (SIN (reduce e2)) 2)
-- (8)
reduce (EADD (EADD e1 e2) e3) = EADD (reduce (EADD e1 e2)) (reduce e3)

reduce (EADD e1 (EADD e2 e3)) = EADD (reduce e1) (reduce (EADD e2 e3))
-- (10)
reduce (EMUL (EMUL e1 e2) e3) = EMUL (reduce (EMUL e1 e2)) (reduce e3)

reduce (EMUL e1 (EMUL e2 e3)) = EMUL (reduce e1) (reduce (EMUL e2 e3))
-- (11)
reduce (EMUL (EADD e1 e2) e3) = EADD (reduce (EMUL e1 e3)) (reduce (EMUL e1 e3))

reduce (EMUL e1 (EADD e2 e3)) = EADD (reduce (EMUL e1 e2)) (reduce (EMUL e1 e3))
-- (6)
reduce (EADD e1 e2) = EADD (reduce e1) (reduce e2)
-- (7, 9, 12)
reduce (EMUL e1 e2)
            | isZero e1 || isZero e2 = zero
            | isOne e1 = reduce e2
            | isOne e2 = reduce e1
            | reduce e1 == reduce e2 = EPOW (reduce e1) 2
            | otherwise = EMUL (reduce e1) (reduce e2)
-- other reductions
reduce (DIV e1 e2) = DIV (reduce e1) (reduce e2)
reduce (D x e)
            | isVAR e = if getVAR e == x
                        then one
                        else zero
            | isPADD e = EADD (D x (POL (getPADDp1 e))) (D x (POL (getPADDp2 e)))
            | isEADD e = EADD (D x (getEADDe1 e)) (D x (getEADDe2 e))
            | isPMUL e = EMUL (num (getPMULc e)) (D x (POL (getPMULp e)))
            | isEMUL e = EADD (EMUL (D x (getEMULe1 e)) (getEMULe2 e)) (EMUL (getEMULe1 e) (D x (getEMULe2 e)))
            | isDIV e = DIV (EADD (EMUL (D x (getDIVe1 e)) (getDIVe2 e)) (neg (EMUL (getDIVe1 e) (D x (getDIVe2 e))))) (EPOW (getDIVe2 e) 2)
            | isPPOW e = POL (PMUL (fromInteger (getPPOWk e)) (PPOW (getPPOWx e) ((getPPOWk e) - 1)))
            | isEPOW e = EMUL (num (fromInteger (getEPOWk e))) (EPOW (getEPOWe e) ((getEPOWk e) - 1))
            | isCOS e = neg (SIN (getCOS e))
            | isSIN e = COS (getSIN e)
            | isNEG e && isCOS (getNEG e) = SIN (getCOS (getNEG e))
            | isNEG e && isSIN (getNEG e) = neg (COS (getSIN (getNEG e)))
reduce (SIN e) = SIN (reduce e)
reduce (COS e) = COS (reduce e)
reduce (POL p) = POL (pReduce p)
reduce (EPOW e k) = EPOW (reduce e) k

-- pReduce
pReduce (VAR x) = VAR x
pReduce (PMUL c p) =
            if c == toRational 1
            then p
            else PMUL c (pReduce p)
pReduce (PADD p1 p2) = PADD (pReduce p1) (pReduce p2)
pReduce (PPOW x k) =
            if k == 1
            then VAR x
            else PPOW x k

-- -- Helper functions
-- VAR
isVAR (POL (VAR x)) = True
isVAR _ = False
getVAR (POL (VAR x)) = x

-- PADD
isPADD (POL (PADD p1 p2)) = True
isPADD _ = False
getPADDp1 (POL (PADD p1 p2)) = p1
getPADDp2 (POL (PADD p1 p2)) = p2

-- EADD
isEADD (EADD e1 e2) = True
isEADD _ = False
getEADDe1 (EADD e1 e2) = e1
getEADDe2 (EADD e1 e2) = e2

-- PMUL
isPMUL (POL (PMUL c p)) = True
isPMUL _ = False
getPMULp (POL (PMUL c p)) = p
getPMULc (POL (PMUL c p)) = c

-- EMUL
isEMUL (EMUL e1 e2) = True
isEMUL _ = False
getEMULe1 (EMUL e1 e2) = e1
getEMULe2 (EMUL e1 e2) = e2

-- DIV
isDIV (DIV e1 e2) = True
isDIV _ = False
getDIVe1 (DIV e1 e2) = e1
getDIVe2 (DIV e1 e2) = e2

-- PPOW
isPPOW (POL (PPOW x k)) = True
isPPOW _ = False
getPPOWx (POL (PPOW x k)) = x
getPPOWk (POL (PPOW x k)) = k

-- EPOW
isEPOW (EPOW e k) = True
isEPOW _ = False
getEPOWe (EPOW e k) = e
getEPOWk (EPOW e k) = k

-- COS
isCOS (COS e) = True
isCOS _ = False
getCOS (COS e) = e

-- SIN
isSIN (SIN e) = True
isSIN _ = False
getSIN (SIN e) = e

-- NEG
isNEG (EMUL (POL (PMUL c (PPOW x 0))) e) = c < 0
isNEG _ = False
getNEG (EMUL (POL (PMUL c (PPOW x 0))) e) = e
neg e = EMUL (POL (PMUL (-1) (PPOW "x" 0))) e

-- Numbers
isNum (POL (PMUL e (PPOW x 0))) = True
isNum _ = False
num e = POL (PMUL e (PPOW "num" 0))

isOne (POL (PPOW x 0)) = True
isOne (EPOW x 0) = True
isOne _ = False
one = POL (PPOW "one" 0)

isZero (POL (PMUL 0 (PPOW x 0))) = True
isZero _ = False
zero = POL (PMUL 0 (PPOW "zero" 0))

-- -- User functions
tEMUL = reduce (EADD (EMUL (POL (PPOW "x" 0))
                           (EPOW (SIN (POL (VAR "x"))) 2))
                     (EMUL (POL (PPOW "x" 0))
                           (EPOW (COS (POL (VAR "x"))) 2)))

t1 = startReducing (DIV (EMUL (EMUL (POL  (PPOW "x" 0))
                                    (EADD (EMUL (SIN (POL (VAR "x")))
                                                (SIN (POL (VAR "x"))))
                                          (EMUL (COS (POL (VAR "x")))
                                                (COS (POL (VAR "x"))))))
                              (POL  (PPOW "x" 3)))
                        (POL  (PPOW "x" 2)))

t2 = reduce (DIV (EMUL (EMUL (POL  (PPOW "x" 0))
                             (EADD (EMUL (SIN (POL (VAR "x")))
                                         (SIN (POL (VAR "x"))))
                                   (EMUL (COS (POL (VAR "x")))
                                         (COS (POL (VAR "x"))))))
                       (POL  (PPOW "x" 3)))
                 (POL  (PPOW "x" 2)))

t2res =      DIV (EMUL (EADD (EMUL (POL (PPOW "x" 0))
                                   (EPOW (SIN (POL (VAR "x"))) 2))
                             (EMUL (POL (PPOW "x" 0))
                                   (EPOW (COS (POL (VAR "x"))) 2)))
                       (POL (PPOW "x" 3)))
                 (POL (PPOW "x" 2))

t3 = reduce (reduce (DIV (EMUL (EMUL (POL  (PPOW "x" 0))
                                    (EADD (EMUL (SIN (POL (VAR "x")))
                                                (SIN (POL (VAR "x"))))
                                          (EMUL (COS (POL (VAR "x")))
                                                (COS (POL (VAR "x"))))))
                              (POL  (PPOW "x" 3)))
                        (POL  (PPOW "x" 2))))

t4 = reduce( reduce (reduce (DIV (EMUL (EMUL (POL  (PPOW "x" 0))
                                    (EADD (EMUL (SIN (POL (VAR "x")))
                                                (SIN (POL (VAR "x"))))
                                          (EMUL (COS (POL (VAR "x")))
                                                (COS (POL (VAR "x"))))))
                              (POL  (PPOW "x" 3)))
                        (POL  (PPOW "x" 2)))))

t5 = reduce (reduce (reduce (reduce (DIV (EMUL (EMUL (POL  (PPOW "x" 0))
                                    (EADD (EMUL (SIN (POL (VAR "x")))
                                                (SIN (POL (VAR "x"))))
                                          (EMUL (COS (POL (VAR "x")))
                                                (COS (POL (VAR "x"))))))
                              (POL  (PPOW "x" 3)))
                        (POL  (PPOW "x" 2))))))

t6 = reduce( reduce( reduce( reduce( reduce (DIV (EMUL (EMUL (POL  (PPOW "x" 0))
                                    (EADD (EMUL (SIN (POL (VAR "x")))
                                                (SIN (POL (VAR "x"))))
                                          (EMUL (COS (POL (VAR "x")))
                                                (COS (POL (VAR "x"))))))
                              (POL  (PPOW "x" 3)))
                        (POL  (PPOW "x" 2)))))))

t1000 = EMUL (POL (PPOW "x" 0)) (EMUL (POL (PMUL 2 (PPOW "x" 0))) (POL (PPOW "x" 0)))