module Tests where

import Expr

-- -- Test cases
t3 = EMUL (POL (PPOW "x" 8))
          (POL (PPOW "x" 2))

t4l = DIV (POL (PPOW "x" 8))
          (POL (PPOW "x" 2))

t4r = DIV (POL (PPOW "x" 2))
          (POL (PPOW "x" 8))

t5 = EADD (EPOW (SIN (POL (VAR "x"))) 2)
          (EPOW (COS (POL (VAR "x"))) 2)

t6l = EADD (num 8)
           (num 2)

t6r = EADD (num 2)
           (num 8)

t7 = EMUL (COS (POL (VAR "x")))
          (COS (POL (VAR "x")))

t8l = EADD (EADD (num 2)
                 (num 8))
           (num 5)

t8r = EADD (num 2)
           (EADD (num 8)
                 (num 5))

t9l = EMUL (num 8)
           (num 2)

t9r = EMUL (num 2)
           (num 8)

t10l = EMUL (EMUL (num 2)
                  (num 8))
            (num 5)

t10r = EMUL (num 2)
            (EMUL (num 8)
                  (num 5))

t11 = EMUL (EADD (num 2)
                 (num 8))
           (num 5)

t12 = EMUL (COS (POL (VAR "x")))
           one

t13 = DIV (EMUL (EMUL (POL  (PPOW "x" 0))
                      (EADD (EMUL (SIN (POL (VAR "x")))
                                  (SIN (POL (VAR "x"))))
                            (EMUL (COS (POL (VAR "x")))
                                  (COS (POL (VAR "x"))))))
                (POL (PPOW "x" 3)))
          (POL (PPOW "x" 2))

t14 = D "x" (EADD (POL (PPOW "x" 4))
                  (D "x" (COS (POL (VAR "x")))))
