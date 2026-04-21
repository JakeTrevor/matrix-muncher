module Main (main) where

import Algebra
import Data.Complex
import Matrix
import MatrixAlgebra
import Permutation
import Quantum
import Typst

-- testExpr :: AExpr
-- testExpr = (AEVar "x" + AEVar "x") * (AEVar "a" + AEVar "b" + AEVar "c") * AEVar "z"

-- testExpr2 :: AExpr
-- testExpr2 = (1 + 2) * (3 + 4 + 5) * (6)

-- testMat :: MExpr
-- testMat = MEVal $ Mat [[AEVar "a", AEVar "b"], [AEVar "c", AEVar "d"]]

-- testMat2 :: MExpr
-- testMat2 = MEVal $ Mat [[AEVar "x", AEVar "y"], [AEVar "k", AEVar "j"]]

xMat :: Mat
xMat = Mat $ [[0, 1], [1, 0]]

xGate :: MExpr
xGate = MEVal xMat

yMat :: Mat
yMat = Mat $ [[0, AEVal (0 :+ (-1))], [AEVal (0 :+ 1), 0]]

yGate :: MExpr
yGate = MEVal yMat

zMat :: Mat
zMat = Mat $ [[1, 0], [0, AEVal (-1)]]

zGate :: MExpr
zGate = MEVal zMat

rt2 :: AExpr
rt2 = AEVar $ "rt2"

rt2_over_2 :: AExpr
rt2_over_2 = rt2 * (AEVal (1 / 2))

nirt2_over_2 :: AExpr
nirt2_over_2 = (AEVal $ (0 :+ (-1))) * rt2_over_2

printMExpr :: MExpr -> IO ()
printMExpr = putStrLn . show . munch

typesetMExpr :: MExpr -> IO ()
typesetMExpr = putStrLn . toTypst . munch

alpha :: Int -> Int -> AExpr
alpha _ 0 = AEVal $ 1
alpha _ 1 = AEVal $ 0 :+ 1
alpha n 2 = AEVal $ -((0 :+ 1) ^ (n + 1))
alpha _ 3 = AEVal $ 0 :+ 1
alpha _ _ = error "alpha only defined in 0..3"

alphaInv :: Int -> Int -> AExpr
alphaInv _ 0 = AEVal $ 1 / 1
alphaInv _ 1 = AEVal $ 1 / (0 :+ 1)
alphaInv n 2 = AEVal $ 1 / (-((0 :+ 1) ^ (n + 1)))
alphaInv _ 3 = AEVal $ 1 / (0 :+ 1)
alphaInv _ _ = error "alpha only defined in 0..3"

sigma :: Int -> MExpr
sigma 0 = idGate 1
sigma 1 = xGate
sigma 2 = yGate
sigma 3 = zGate
sigma _ = error "sigma only defined in 0..3"

sigmaT :: Int -> MExpr
sigmaT 0 = idGate 1
sigmaT 1 = xGate
sigmaT 2 = MEVal $ matTrans yMat
sigmaT 3 = zGate
sigmaT _ = error "sigmaT only defined in 0..3"

enc :: Int -> MExpr
enc n =
  let dim = (2 * n) + 1
      xSig = n `copies` (xGate *^* idGate 1)
      zSig = n `copies` (zGate *^* idGate 1)

      rt2_id = rt2_over_2 @ idGate 1
      xMsg = rt2_id + (nirt2_over_2 @ xGate)
      zMsg = rt2_id + (nirt2_over_2 @ zGate)

      x_component = xMsg *^* xSig
      z_component = zMsg *^* zSig
   in x_component * z_component

enc' :: Int -> MExpr
enc' n =
  ( AEVal (1 / 2)
      @ MEOp
        MatPlus
        [ MEOp MatKronecker ([(alphaInv n m) @ (sigma m)] ++ [sigma m *^* idGate 1 | _ <- [0 .. n - 1]])
        | m <- [0 .. 3]
        ]
  )

bell_phi_plus :: MExpr
bell_phi_plus = MEVal $ Mat [[1, 0, 0, 1]]

phi_mu_ket :: Int -> MExpr
phi_mu_ket m = ((sigma m) *^* idGate 1) * bell_phi_plus

phi_mu :: Int -> MExpr
phi_mu m =
  let p = phi_mu_ket m
      pt = MEConj p
   in p * pt

-- Decode the message onto the t'th signal bit
dec :: Int -> Int -> MExpr
dec n t =
  let dim = (2 * n) + 1
      p = MEVal $ permGate dim [0, t, t + 1]
      pInv = MEVal $ permGateInv dim [0, t, t + 1]
      components = [idGate 1 *^* ((alpha n m) @ phi_mu m) *^* MEOp MatKronecker [idGate 1 *^* sigmaT m | _ <- [0 .. n - 1]] | m <- [0 .. 3]]
   in p * ((AEVal (1 / 2)) @ MEOp MatPlus components) * pInv

main :: IO ()
main = do
  putStrLn "#set page(width: 3000pt, height: 1000pt)"
  putStrLn "$"
  putStrLn "I^* = "
  typesetMExpr $ MEConj $ idGate 1
  putStrLn "\\ X^* = "
  typesetMExpr $ MEConj $ xGate
  putStrLn "\\ Y^* = "
  typesetMExpr $ MEConj $ yGate
  putStrLn "\\ Z^* = "
  typesetMExpr $ MEConj $ zGate
  putStrLn "$"
  putStrLn "$"
  putStrLn "\"enc\"(2) = "
  typesetMExpr $ enc 2
  putStrLn "$"
  putStrLn "$"
  putStrLn "\"enc'\"(2) = "
  typesetMExpr $ enc' 2
  putStrLn "$"

  putStrLn "$"
  putStrLn "|phi.alt_0 chevron.r chevron.l phi.alt_0| = "
  typesetMExpr $ phi_mu 0
  putStrLn "\\ |phi.alt_1 chevron.r chevron.l phi.alt_1|  = "
  typesetMExpr $ phi_mu 1
  putStrLn "\\ |phi.alt_2 chevron.r chevron.l phi.alt_2| = "
  typesetMExpr $ phi_mu 2
  putStrLn "\\ |phi.alt_3 chevron.r chevron.l phi.alt_3. | = "
  typesetMExpr $ phi_mu 3
  putStrLn "$"

  putStrLn "$"
  putStrLn "\"dec\"(n, 1) = "

  --   typesetMExpr $ dec 2 1
  putStrLn "$"