module Main where

-- -- Ask for user input
main = start
start = do
        putStrLn "Hello, please specify an expression."
        expr <- getLine
        putStrLn ("Hey " ++ expr ++ ", you rock!")

-- -- Data types
type K = Integer
type C = Rational
type X = String
data P = VAR X | PADD P P | PMUL C P | PPOW X K deriving (Eq, Show)
data E = POL P | EADD E E | EMUL E E | EPOW E K | DIV E E | D X E | SIN E | COS E deriving (Eq, Show)

-- -- initiate
-- Function to start reduction
startReducing e = do
                  putStrLn "\nInitial expression: "
                  putStrLn (removeExcessChars (show e))
                  reducing e

-- Function to iteratively reduce if more reductions can be applied
reducing e = let reduced = reduce e
             in if e == reduced
                then do
                     putStrLn "No further reductions can be applied. Resulting expression:"
                     putStrLn (removeExcessChars (show e))
                else do
                     putStrLn "Applying reductions:"
                     putStrLn (removeExcessChars (show reduced))
                     reducing reduced

removeExcessChars xs = [ x | x <- xs, x `notElem` "\"" ]

-- -- Test cases
t3 = startReducing (EMUL (POL (PPOW "x" 8))
                         (POL (PPOW "x" 2)))

t4 = startReducing (DIV (POL (PPOW "x" 8))
                        (POL (PPOW "x" 2)))

t5 = startReducing (EADD (EPOW (SIN (POL (VAR "x"))) 2)
                         (EPOW (COS (POL (VAR "x"))) 2))

t6l = startReducing (EADD (num 8)
                          (num 2))

t6r = startReducing (EADD (num 2)
                          (num 8))

t7 = startReducing (EMUL (COS (POL (VAR "x")))
                         (COS (POL (VAR "x"))))

t8l = startReducing (EADD (EADD (num 2)
                                (num 8))
                          (num 5))

t8r = startReducing (EADD (num 2)
                          (EADD (num 8)
                                (num 5)))

t9l = startReducing (EMUL (num 8)
                          (num 2))

t9r = startReducing (EMUL (num 2)
                          (num 8))

t10l = startReducing (EMUL (EMUL (num 2)
                                 (num 8))
                           (num 5))

t10r = startReducing (EMUL (num 2)
                           (EMUL (num 8)
                                 (num 5)))

t11 = startReducing (EMUL (EADD (num 2)
                                (num 8))
                          (num 5))

t12 = startReducing (EMUL one
                          (COS (POL (VAR "x"))))

t13 = startReducing (DIV (EMUL (EMUL (POL  (PPOW "x" 0))
                                     (EADD (EMUL (SIN (POL (VAR "x")))
                                                 (SIN (POL (VAR "x"))))
                                           (EMUL (COS (POL (VAR "x")))
                                                 (COS (POL (VAR "x"))))))
                               (POL (PPOW "x" 3)))
                         (POL (PPOW "x" 2)))

t14 = startReducing (D "x" (EADD (POL (PPOW "x" 4))
                                 (D "x" (COS (POL (VAR "x"))))))

-- -- Reductions
-- Reduce expressions
reduce (COS e) = COS (reduce e)
reduce (SIN e) = SIN (reduce e)
reduce (EPOW e k) = EPOW (reduce e) k
reduce (POL p) = POL (pReduce p)
reduce (D x eIN)
            | isVAR e && getVAR e == x = one
            | isVAR e && getVAR e /= x = zero
            | isCOS e = neg (SIN (getCOS e))
            | isSIN e = COS (getSIN e)
            | isNEG e && isCOS (getNEG e) = SIN (getCOS (getNEG e))
            | isNEG e && isSIN (getNEG e) = neg (COS (getSIN (getNEG e)))
            | isPADD e = EADD (D x (POL (getPADDp1 e))) (D x (POL (getPADDp2 e)))
            | isEADD e = EADD (D x (getEADDe1 e)) (D x (getEADDe2 e))
            | isPMUL e = EMUL (num (getPMULc e)) (D x (POL (getPMULp e)))
            | isEMUL e = EADD (EMUL (D x (getEMULe1 e)) (getEMULe2 e)) (EMUL (getEMULe1 e) (D x (getEMULe2 e)))
            | isDIV e = DIV (EADD (EMUL (D x (getDIVe1 e)) (getDIVe2 e)) (neg (EMUL (getDIVe1 e) (D x (getDIVe2 e))))) (EPOW (getDIVe2 e) 2)
            | isPPOW e = POL (PMUL (fromInteger (getPPOWk e)) (PPOW (getPPOWx e) (getPPOWk e - 1)))
            | isEPOW e = EMUL (num (fromInteger (getEPOWk e))) (EPOW (getEPOWe e) (getEPOWk e - 1))
            where e = reduce eIN
reduce (DIV e1IN e2IN)
            | isPPOW e1 && isPPOW e2 && getPPOWx e1 == getPPOWx e2 = POL (PPOW (getPPOWx e1) (getPPOWk e1 - getPPOWk e2))
            | isEPOW e1 && isEPOW e2 && getEPOWe e1 == getEPOWe e2 = EPOW e1 (getEPOWk e1 - getEPOWk e2)
            | otherwise = DIV e1 e2
            where e1 = reduce e1IN
                  e2 = reduce e2IN
reduce (EMUL e1IN e2IN)
            | isPPOW e1 && isPPOW e2 && getPPOWx e1 == getPPOWx e2 = POL (PPOW (getPPOWx e1) (getPPOWk e1 + getPPOWk e2))
            | isEPOW e1 && isEPOW e2 && getEPOWe e1 == getEPOWe e2 = EPOW e1 (getEPOWk e1 + getEPOWk e2)
            | isZero e1 || isZero e2 = zero
            | isOne e1 = e2
            | isOne e2 = e1
            | e1 == e2 = EPOW e1 2
            | isEADD e1 && e1 == reduce e1 = EADD (EMUL (getEADDe1 e1) e2) (EMUL (getEADDe2 e1) e2)
            | isEADD e2 && e2 == reduce e2 = EADD (EMUL e1 (getEADDe1 e2)) (EMUL e1 (getEADDe2 e2))
            | isNum e1 && isNum e2 = num (getNum e1 * getNum e2)
            | otherwise = EMUL e1 e2
            where e1 = reduce e1IN
                  e2 = reduce e2IN
reduce (EADD e1IN e2IN)
            | isEMUL e1 && isEMUL e2 &&
              ((isSIN (getEMULe1 e1) && isSIN (getEMULe2 e1) && isCOS (getEMULe1 e2) && isCOS (getEMULe2 e2) &&
                getVAR (getSIN (getEMULe1 e1)) == getVAR (getSIN (getEMULe2 e1)) &&
                getVAR (getSIN (getEMULe2 e1)) == getVAR (getCOS (getEMULe1 e2)) &&
                getVAR (getCOS (getEMULe1 e2)) == getVAR (getCOS (getEMULe2 e2))) ||
               (isCOS (getEMULe1 e1) && isCOS (getEMULe2 e1) && isSIN (getEMULe1 e2) && isSIN (getEMULe2 e2) &&
                getVAR (getCOS (getEMULe1 e1)) == getVAR (getCOS (getEMULe2 e1)) &&
                getVAR (getCOS (getEMULe2 e1)) == getVAR (getSIN (getEMULe1 e2)) &&
                getVAR (getSIN (getEMULe1 e2)) == getVAR (getSIN (getEMULe2 e2)))) = one
            | isEPOW e1 && isEPOW e2 &&
              ((isSIN (getEPOWe e1) && isCOS (getEPOWe e2) &&
                reduce (getSIN (getEPOWe e1)) == reduce (getCOS (getEPOWe e2))) ||
               (isCOS (getEPOWe e1) && isSIN (getEPOWe e2) &&
                reduce (getCOS (getEPOWe e1)) == reduce (getSIN (getEPOWe e2)))) &&
              getEPOWk e1 == 2 && getEPOWk e2 == 2 = one
            | isNum e1 && isNum e2 = num (getNum e1 + getNum e2)
            | otherwise = EADD e1 e2
            where e1 = reduce e1IN
                  e2 = reduce e2IN

-- Reduce polynominals
pReduce (VAR x) = VAR x
pReduce (PADD p1 p2) = PADD (pReduce p1) (pReduce p2)
pReduce (PMUL c p)
            | c == toRational 1 = p
            | otherwise = PMUL c (pReduce p)
pReduce (PPOW x k)
            | k == 1 = VAR x
            | otherwise = PPOW x k

-- -- is, get and instantiate functions
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
neg e = EMUL (num (-1)) e

-- Numbers
isNum (POL (PMUL e (PPOW x 0))) = True
isNum (POL (PPOW x 0)) = True
isNum _ = False
returnNum (POL (PMUL e (PPOW x 0))) = e
getNum n = if isOne n
           then 1
           else returnNum n
num e = POL (PMUL e (PPOW "num" 0))

isOne (POL (PPOW x 0)) = True
isOne (EPOW x 0) = True
isOne _ = False
one = POL (PPOW "one" 0)

isZero (POL (PMUL 0 (PPOW x 0))) = True
isZero _ = False
zero = POL (PMUL 0 (PPOW "zero" 0))