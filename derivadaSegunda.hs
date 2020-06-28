module DerivadaSegunda (
  Op,
  evalua,
  derivaPunto,
  deriva,
  derSup,
  coefTaylor,
  taylor,
  mcLaurin ) where

{-
He creado un tipo de dato llamado Op (Operacion) que consiste en el tipo de las
funciones reales de variable real. Es un tipo recursivo donde
los casos bases son la funcion identidad y las constantes reales.
No hay necesidad de evaluar la regla de la cadena ya que esta ya está
implicitamente puesta en la definicion de derivada.
Las instancias show de cada funcion deben ser
simpificadas para reducir lo que se imprime en pantalla.
Actualemente la forma en la que se muestran en pantallas es
horrible y dificil de leer. A parte, la forma en la que se introduce
las funciones en la consola tambien es horrible.
-}
------------------------------------------------------------
--IMPLEMENTACION DE LAS FUNCIONES DE VARIABLE REAL COMO
--ESTRUCTURA DE DATOS RECURSIVA.

data Op = Paren Op|                         --(op)
          Sum Op Op|                        --Suma de funciones
          Resta Op Op|                      --resta
          Multi Op Op|                      --producto
          Divi Op Op|                       --cociente
          Expo Op|                          --e ^f(x)
          Logar Op|                         --Log(f(x))
          Superexpo Op Op|                  --f(x)^g(x)
          Constant Double Op|               --k*f(x)
          Composicion Op Op|                --f(g(x))
          Eleva Double Op|                  --(f(x))^k
          Sen Op|                           --sen(f(x))
          Cos Op|                           --cos(f(x))
          Tng Op|                           --tng(f(x))
          Entero Double|                    --k   CASO ULTIMO
          Senh Op|                          --senh(f(x))
          Cosh Op|                          --cosh(f(x))
          Tngh Op|                          -- Tangente hiperbolica
          Iden                              --identidad  CASO ULTIMO

instance Show Op where
  show (Sum f1 f2) = (show f1)++" + "++(show f2)
  show (Resta f1 f2) = (show f1)++" - "++(show f2)
  show (Multi f1 f2) = (show f1)++" * "++(show f2)
  show (Divi f1 f2) = (show f1)++" / "++(show f2)
  show (Expo f1) = "e^"++ "(" ++ (show f1) ++ ")"
  show (Logar f1) = "Log "++"( "++(show f1)++" )"
  show (Superexpo f1 f2) = "("++show f1++")"++"^"++"("++show f2++")"
  show (Paren op) = "("++show op++")"
  show (Constant n f) = if n == 1 then show f else (show n)++"* ("++(show f)++")"
  show (Composicion f1 f2) = (show f1)++"º"++(show f2)
  show (Eleva k f) = "("++show f++")"++"^"++"("++show k++")"
  show (Sen f) = "Sen("++(show f)++")"
  show (Cos f) = "Cos("++(show f)++")"
  show (Tng f)  = "Tng("++(show f)++")"
  show (Entero n)  = show n
  show (Senh f) = "Senh("++(show f)++")"
  show (Cosh f) = "Cosh("++(show f)++")"
  show (Tngh f) = "Tngh("++show f++")"
  show Iden = "x"

--FUNCIONES AUXILIARES

--Dada una funcion f :: Op y un numero a :: Double, ver que valor tiene f a :: Double

evalua :: Op -> Double -> Double
evalua (Paren f) a = evalua f a
evalua (Sum f1 f2) a = (evalua f1 a) + (evalua f2 a)
evalua (Resta f1 f2) a = evalua f1 a - evalua f2 a
evalua (Multi (f1) (f2)) a =  (evalua f1 a)*(evalua f2 a)
evalua (Divi f1 f2) a = (evalua f1 a) / (evalua f2 a)
evalua (Expo f) a = exp(evalua f a)
evalua (Logar f) a = log (evalua f a)
evalua (Superexpo f1 f2) a = (evalua f1 a)**(evalua f2 a)
evalua (Constant k f) a = k*(evalua f a)
evalua (Eleva k f) a = (evalua f a)**k
evalua (Sen f) a = sin (evalua f a)
evalua (Cos f) a = cos (evalua f a)
evalua (Tng f) a = tan (evalua f a)
evalua (Entero k) _ = k
evalua (Senh f) a = sinh (evalua f a)
evalua (Cosh f) a = cosh (evalua f a)
evalua (Tngh f) a = tanh (evalua f a)
evalua Iden a = a

--Funcion factorial. Usada más tarde en los polinomios

fact :: Integer -> Integer
fact 0 = 1
fact n = foldr (*) 1 [1..n]
 
--DEFINICIONES DE LAS DERIVADAS

--Definicion de  la derivada en un punto
derivaPunto :: Op -> Double -> Double
derivaPunto f a = ((evalua f (a+0.000001))-(evalua f a))/(0.000001)

--Definicion de derivada de manera no analitica.

deriva :: Op -> Op
deriva (Paren f) = Paren (deriva f)
deriva (Sum f1 f2) = Sum (deriva f1) (deriva f2)
deriva (Resta f1 f2) = Resta (deriva f1) (deriva f2)
deriva (Multi (f1) (f2)) = Sum (Multi (deriva f1) (f2)) (Multi (f1) (deriva f2))
deriva (Divi f1 f2) = Divi (Resta (Multi (deriva f1) f2) (Multi f1 (deriva f2))) (Eleva 2 f2)
deriva (Expo f) = Multi (Expo f) (deriva f)
deriva (Logar f) = Divi (deriva f) (f)
deriva (Superexpo f1 f2) = Multi (Superexpo f1 f2) (deriva (Multi (f2) (Logar f1)) )
deriva (Constant k f) = Constant k (deriva f)
deriva (Eleva k f) = Multi (Constant k (Eleva (k-1) f)) (deriva f)
deriva (Sen f) = Multi (Cos f) (deriva f)
deriva (Cos f) = Multi (Constant (-1) (Sen f)) (deriva f)
deriva (Tng f) = Multi (Sum (Entero 1) (Eleva 2 (Tng f))) (deriva f)
deriva (Entero _) = Entero 0
deriva (Senh f) = Multi (Cosh f) (deriva f)
deriva (Cosh f) = Multi (Senh f) (deriva f)
deriva (Tngh f) = Multi (Constant (-1) (deriva (Eleva (-1) (Cosh f)))) (deriva f)
deriva Iden = Entero 1

--definicion de las derivadas de orden superior
derSup :: Integer -> Op -> Op
derSup 0 op = op
derSup n op = derSup (n-1) (deriva op)

-- evaluar un valor en las derivadas sucesivas de una funcion. Funcion auxiliar.
derSupPto :: Integer -> Double -> Op -> Double
derSupPto n a op = evalua (derSup n op) a

--POLINOMIOS DE TAYLOR Y DE MCLAURIN

--Coeficientes de Taylor de f en a. Corresponden a (f^(k)(a))/k!
coefTaylor :: Op -> Integer -> Double -> [Double]
coefTaylor f n a = [(derSupPto k a f)/(fromIntegral (fact k)) | k<-[0..n]]

--Cada elementode la lista son los sumandos de sum (f^(k)(a))/k! * (x-a)^k
--que es la definicion del polinomio de taylor en a de grado n.
taylorAux :: Op -> Double -> Integer -> [Op]
taylorAux f a n = let xs = [(evalua (derSup k f) a)/fromIntegral(fact k) | k<-[0..n]]
                      ys = [Eleva (fromIntegral k) (Resta Iden (Entero a)) | k<-[0..n]]
                  in zipWith (\x y -> Constant x y) xs ys

--Da el polinomio de taylor como un Op
taylor :: Op -> Double -> Integer -> Op
taylor f a n = foldr (\ x xs -> Sum x xs) (Entero 0) (taylorAux f a n)

--Polinomio de mcLaurin
mcLaurin :: Op -> Integer -> Op
mcLaurin f n = taylor f 0 n

