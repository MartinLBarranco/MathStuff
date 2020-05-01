
--IMPLEMENTACION DE LAS FRACCIONES CONTINUAS EN HASKELL

module FraccionesContinuas
   (FracCont,
    FracCLi,
    pasa2List,
    list2Frac,
    evalua2Num,
    pasa2Frac --undefined:es necesario pasar un Float a (a,b) donde a/b=ese float
    ) where

    
-- ============================================================================
data FracCont = FracTri Int | Frac (Int, FracCont) --Definición declarativa

type FracCLi = [Int]  --Definición con lista (Es meramente auxiliar)

instance Show FracCont where
  show (FracTri a) = show [a]
  show (Frac (b, f)) = show [b] ++ show f
-- ============================================================================

--Pasa una fraccion a lista

pasa2List :: FracCont -> FracCLi
pasa2List (FracTri a) = [a]
pasa2List (Frac (a,f)) = a : pasa2List f

-- ============================================================================

--Pasa una lista a fraccion

list2Frac :: FracCLi -> FracCont
list2Frac [] = error("Esto no está definido todavía")
list2Frac [x] = FracTri x
list2Frac (x:xs) = Frac (x, (list2Frac xs))

-- ============================================================================

--Ejemplos de fracciones continuas:

ej1, ej2, ej3, ej4, ej5, ej6, ej7, ej8, ej9, ej10 :: FracCont
ej1 = list2Frac [4,8,2,9,6,7]
ej2 = list2Frac [1,1,1,1,1,1]
ej3 = list2Frac [2,9,6,1,7,5]
ej4 = list2Frac [0,0,0,0,0,1]
ej5 = list2Frac [7,8,4,5,9,6]
ej6 = list2Frac [6]
ej7 = list2Frac [8]
ej8 = list2Frac [1]
ej9 = list2Frac [1000000000000]
ej10 = list2Frac [8652968]

-- ============================================================================

--Calculael valor de una fraccion continua

evalua2Num :: FracCont -> Double
evalua2Num (FracTri a) = 1/(fromIntegral a)
evalua2Num(Frac (a,f)) = 1/((fromIntegral a)+(evalua2Num f))

-- ============================================================================
--Pasa un número entre (1,-1) menos el cero. Si no es así, le quita la parte
  --entera.

pasa2Frac :: Float -> FracCont
pasa2Frac x | x == 0 = error("El numero es cero")
            | x>=1 || x<=(-1) = pasa2Frac (x-fromIntegral(truncate x))
            | otherwise = pasaAux x

pasaAux :: Float -> FracCont
pasaAux x = undefined

