module Eval where

import Expr

-- -- Reductions
-- Reduce expressions
reduce (COS e) = COS (reduce e)
reduce (SIN e) = SIN (reduce e)
reduce (EPOW e k)
            | k == 1 = reduce e
            | otherwise = EPOW (reduce e) k
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
            | isDIV e = DIV (EADD (EMUL (D x (getDIVe1 e)) (getDIVe2 e)) (neg (EMUL (getDIVe1 e) (D x (getDIVe2 e)))))
                            (EPOW (getDIVe2 e) 2)
            | isPPOW e = POL (PMUL (fromInteger (getPPOWk e)) (PPOW (getPPOWx e) (getPPOWk e - 1)))
            | isEPOW e = EMUL (num (fromInteger (getEPOWk e))) (EPOW (getEPOWe e) (getEPOWk e - 1))
            where e = reduce eIN
reduce (DIV e1IN e2IN)
            | isPPOW e1 && isPPOW e2 &&
              getPPOWx e1 == getPPOWx e2 = if (getPPOWk e1 - getPPOWk e2) >= 0
                                           then POL (PPOW (getPPOWx e1) (getPPOWk e1 - getPPOWk e2))
                                           else DIV one (POL (PPOW (getPPOWx e1) (-1 * (getPPOWk e1 - getPPOWk e2))))
            | isEPOW e1 && isEPOW e2 &&
              getEPOWe e1 == getEPOWe e2 = if (getEPOWk e1 - getEPOWk e2) >= 0
                                           then EPOW (getEPOWe e1) (getEPOWk e1 - getEPOWk e2)
                                           else DIV one (EPOW (getEPOWe e1) (-1 * (getEPOWk e1 - getEPOWk e2)))
            | otherwise = DIV e1 e2
            where e1 = reduce e1IN
                  e2 = reduce e2IN
reduce (EMUL e1IN e2IN)
            | isPPOW e1 && isPPOW e2 && getPPOWx e1 == getPPOWx e2 = POL (PPOW (getPPOWx e1)
                                                                               (getPPOWk e1 + getPPOWk e2))
            | isEPOW e1 && isEPOW e2 && getEPOWe e1 == getEPOWe e2 = EPOW (getEPOWe e1) (getEPOWk e1 + getEPOWk e2)
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
pReduce (PADD p1 p2)
            | isNum (POL p1) && isNum (POL p2) = PMUL (getNum (POL p1) + getNum (POL p2)) (PPOW "num" 0)
            | otherwise = PADD (pReduce p1) (pReduce p2)
pReduce (PMUL c p)
            | c == toRational 1 = p
            | isPMUL (POL p) = PMUL (c * getPMULc (POL p)) (getPMULp (POL p))
            | otherwise = PMUL c (pReduce p)
pReduce (PPOW x k)
            | k == 1 = VAR x
            | otherwise = PPOW x k
