module Expr where

source :: E -> String
source e = case e of
  (EADD e1 e2) -> if isNEG e2
                  then parens $ source e1 ++ " - " ++ source (getNEG e2)
                  else parens $ source e1 ++ " + " ++ source e2
  (EMUL e1 e2) -> if isNum e1 && isNum e2
                  then show (fromRational (getNum e1 * getNum e2))
                  else parens $ source e1 ++ " * " ++ source e2
  (EPOW e k) -> if k == 0
                then "1"
                else parens $ source e ++ "^" ++ show k
  (DIV e1 e2) -> parens $ source e1 ++ " / " ++ source e2
  (D x e2) -> parens $ "d/d" ++ x ++ parens (source e2)
  (SIN e) -> parens $ "sin" ++ parens (source e)
  (COS e) -> parens $ "cos" ++ parens (source e)
  (POL p) -> pSource p
  where parens s = "(" ++ s ++ ")"

pSource :: P -> String
pSource p = case p of
  (PADD p1 p2) -> parens $ pSource p1 ++ " + " ++ pSource p2
  (PMUL c p) -> if isNum (POL p)
                then show (fromRational (c * getNum (POL p)))
                else parens $ show (fromRational c) ++ " * " ++ pSource p
  (PPOW x k) -> if k == 0
                then "1"
                else parens $ x ++ "^" ++ show k
  (VAR x) -> x
  where parens s = "(" ++ s ++ ")"

-- -- Data types
type K = Integer
type C = Rational
type X = String
data P = VAR X | PADD P P | PMUL C P | PPOW X K deriving (Eq, Show)
data E = POL P | EADD E E | EMUL E E | EPOW E K | DIV E E | D X E | SIN E | COS E deriving (Eq, Show)

-- -- is, get and instantiate functions
-- POL
isPOL (POL p) = True
isPOL _ = False
getPOL (POL p) = p

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