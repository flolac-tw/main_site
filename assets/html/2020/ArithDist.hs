
import Test.Hspec
import Control.Monad

data Expr =
    ILit Integer
  | Plus Expr Expr
  | Times Expr Expr
  | BLit Bool
  | Equ Expr Expr
  | If Expr Expr Expr
  | Var String
  | Let String Expr Expr
  deriving (Show, Eq)

isValue :: Expr -> Bool
isValue (ILit _) = True
isValue (BLit _) = True
isValue _        = False

redex :: Expr -> [Expr]
redex (Plus (ILit n) (ILit m))  = [ILit (n + m)]
redex (Times (ILit n) (ILit m)) = [ILit (n * m)]
redex _                         = []

reduce :: Expr -> [Expr]
reduce e = redex e ++
           case e of
             ILit _      -> []
             Plus e1 e2  -> [ Plus e1' e2   | e1' <- reduce e1 ] ++
                            [ Plus e1  e2'  | isValue e1, e2' <- reduce e2 ]
             Times e1 e2 -> [ Times e1' e2  | e1' <- reduce e1 ] ++
                            [ Times e1  e2' | isValue e1, e2' <- reduce e2 ]

{-|
  The `subst` function substitutes the given value for the designated variable
  in an expression. More precisely,
  * The expression`subst e v x` means substitute `v` for `x` in `e`
  * The `subst` function is limited to the case where
    `v` is either an `ILit` or a `BLit`.
-}
subst :: Expr -> Expr -> String -> Maybe Expr
subst e v x | isValue v = Just (subst' e v x)
subst _ _ _             = Nothing

subst' :: Expr -> Expr -> String -> Expr
subst' oldExpr newValue var = undefined


{- ------------------------------------------------------ -
 - ------------------------------------------------------ -
 -                                                        -
 -                          tests                         -
 -                                                        -
 - ------------------------------------------------------ -
 - ------------------------------------------------------ -}

data RedexConfig = RedexConfig { name :: String
                               , relation :: Expr -> [Expr]
                               , recurs :: Bool }

test_redexes :: RedexConfig -> SpecWith (Arg Expectation)
test_redexes RedexConfig{name = name, relation = redex, recurs = willRecur} = do
  describe ("‘" ++ name ++ "’ should include") $ do
    it "basic arithmetic operations: numbers" $ do
      redex (Plus (ILit 3) (ILit 8))
        `shouldBe`
        [ILit 11]
      redex (Times (ILit 3) (ILit 8))
        `shouldBe`
        [ILit 24]
    it "let substitution" $ do
      redex (Let "x" (ILit 3)
                 (Let "y" (ILit 4)
                      (Plus (Times (Var "y") (Var "y"))
                            (Times (Var "x") (Var "x")))))
        `shouldBe`
        [Let "y" (ILit 4)
             (Plus (Times (Var "y") (Var "y"))
                   (Times (ILit 3) (ILit 3)))]
    it "basic arithmetic operations: equality" $ do
      redex (Equ (ILit 3) (ILit 8))
        `shouldBe`
        [BLit False]
      redex (Equ (ILit 7) (ILit 7))
        `shouldBe`
        [BLit True]
      redex (Equ (BLit False) (BLit False))
        `shouldBe`
        [BLit True]
    it "conditional branching" $ do
      redex (If (BLit True) (ILit 5) (Times (ILit 2) (ILit 77)))
        `shouldBe`
        [ILit 5]
      redex (If (BLit False) (ILit 5) (Times (ILit 2) (ILit 77)))
        `shouldBe`
        [Times (ILit 2) (ILit 77)]

  describe ("‘" ++ name ++ "’ specifies") $
    it "call-by-value semantics" $ do
      redex (Let "x" (Var "y") (Var "x"))
        `shouldBe`
        []
      unless willRecur $
        redex (Let "x" (Plus (ILit 5) (ILit 0)) (Var "x"))
          `shouldBe`
          []

test_reduce :: SpecWith (Arg Expectation)
test_reduce = do
  describe "‘reduce’ includes ‘redex’" $
    test_redexes $ RedexConfig{ name = "reduce", relation = reduce, recurs = True}
  describe "‘reduce’ specifies left-to-right call-by-value evaluation strategy" $ do
    describe "for arithmetic operations:" $ do
      it "covers values and stuck terms" $ do
        reduce (ILit 5)
          `shouldBe`
          []
      it "evaluate subterms from left to right" $ do
        reduce (Plus (ILit 5) (ILit 13))
          `shouldBe`
          [ILit 18]
        reduce (Plus (Times (ILit 5) (ILit 2))
                    (Plus (ILit 5) (ILit 2)))
          `shouldBe`
          [Plus (ILit 10)
                (Plus (ILit 5) (ILit 2))]
        reduce (Plus (ILit 10)
                    (Plus (ILit 5) (ILit 2)))
          `shouldBe`
          [Plus (ILit 10) (ILit 7)]
        reduce (Times (ILit 5) (ILit 13))
          `shouldBe`
          [ILit 65]
        reduce (Times (Times (ILit 5) (ILit 2))
                      (Plus (ILit 5) (ILit 2)))
          `shouldBe`
          [Times (ILit 10)
                 (Plus (ILit 5) (ILit 2))]
        reduce (Times (ILit 10)
                      (Plus (ILit 5) (ILit 2)))
          `shouldBe`
          [Times (ILit 10) (ILit 7)]
    describe "for let expressions:" $ do
      it "covers values and stuck terms" $ do
        reduce (Var "z")
          `shouldBe`
          []
        reduce (Plus (ILit 5) (Var "s"))
          `shouldBe`
          []
      it "evaluate ‘let’ satisfying the by-value strategy" $ do
        reduce (Let "x" (ILit 8) (Var "x"))
          `shouldBe`
          [ILit 8]
        reduce (Let "x" (Times (ILit 5) (ILit 13)) (Var "x"))
          `shouldBe`
          [Let "x" (ILit 65) (Var "x")]
    describe "for conditional expressios:" $ do
      it "covers values and stuck terms" $ do
        reduce (BLit False)
          `shouldBe`
          []
      it "evaluate subterms from left to right" $ do
        reduce (Equ (Times (ILit 6) (ILit 4))
                    (Times (ILit 8) (ILit 3)))
          `shouldBe`
          [Equ (ILit 24) (Times (ILit 8) (ILit 3))]
        reduce (Equ (ILit 24) (Times (ILit 8) (ILit 3)))
          `shouldBe`
          [Equ (ILit 24) (ILit 24)]
      it "evaluate ‘if’ properly" $ do
        reduce (If (Equ (ILit 5) (ILit 8))
                   (Plus (ILit 2) (ILit 3))
                   (Times (ILit 4) (ILit 5)))
          `shouldBe`
          [If (BLit False)
              (Plus (ILit 2) (ILit 3))
              (Times (ILit 4) (ILit 5))]
        reduce (If (Equ (ILit 3) (ILit 3))
                   (Plus (ILit 2) (ILit 3))
                   (Times (ILit 4) (ILit 5)))
          `shouldBe`
          [If (BLit True)
              (Plus (ILit 2) (ILit 3))
              (Times (ILit 4) (ILit 5))]

test_subst :: SpecWith (Arg Expectation)
test_subst = do
  describe "The ‘subst’ function" $ do
    it "fails when substituting non-literals" $
      subst (ILit 0) (Plus (ILit 0) (ILit 0)) "x"
            `shouldBe`
            Nothing
    it "substitutes new value for matching variables" $ do
      subst (Var "x") (ILit 5) "x"
            `shouldBe`
            Just (ILit 5)
      subst (Var "y") (ILit 5) "x"
            `shouldBe`
            Just (Var "y")
    it "recurs structurally" $ do
      subst (ILit 7) (ILit 5) "x"
            `shouldBe`
            Just (ILit 7)
      subst (Plus (ILit 8) (Times (Var "x") (Var "x"))) (ILit 5) "x"
            `shouldBe`
            Just (Plus (ILit 8) (Times (ILit 5) (ILit 5)))
      subst (Let "x" (Var "x") (ILit 8)) (ILit 5) "x"
            `shouldBe`
            Just (Let "x" (ILit 5) (ILit 8))
    it "shadowed by new let bindings" $
      subst (Let "x" (ILit 8) (Var "x")) (ILit 5) "x"
            `shouldBe`
            Just (Let "x" (ILit 8) (Var "x"))
    it "recurs through non-shadowing let expressions" $
      subst (Let "y" (ILit 8) (Var "x")) (ILit 5) "x"
            `shouldBe`
            Just (Let "y" (ILit 8) (ILit 5))
  describe "handles booleans, equality and conditional expressions:" $ do
    it "recurs structurally" $ do
      subst (BLit False) (ILit 5) "x"
            `shouldBe`
            Just (BLit False)
      subst (Equ (Var "x") (ILit 5)) (ILit 7) "x"
            `shouldBe`
            Just (Equ (ILit 7) (ILit 5))
      subst (Plus (ILit 8) (If (Equ (Var "x") (ILit 5))
                               (ILit 99)
                               (Var "x")))
            (ILit 5) "x"
            `shouldBe`
            Just (Plus (ILit 8) (If (Equ (ILit 5) (ILit 5))
                                    (ILit 99)
                                    (ILit 5)))

main =
  hspec $ do
    test_redexes $ RedexConfig{ name = "redex", relation = redex, recurs = False }
    test_reduce
    test_subst
