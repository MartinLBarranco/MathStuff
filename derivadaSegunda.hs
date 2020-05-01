module DerivadaSegunda
(deriva,
 derSup,
 taylor,
 mcLaurin,
 coefTaylor,
 evalua,
 coefLaurin) where







{-
He creado una clase llamado Op (Operacion) que consiste en el tipo de las
funciones reales de variable real. Es un tipo recursivo donde
los casos bases son la funcion identidad y las constantes.
Tambien he definido el tipo polinomio para trabajar con ellos
para trabajar comodamente con taylor.Ademas, trabajar con
polinomios en esta notacion es tedioso.
No hay necesidad de evaluar la regla de la cadena ya que esta ya está
implicitamente puesta en el codigo. La derivada de una funcion elevada a otra
no la he puedto por pereza. Las instancias show de cada funcion deben ser
simpificadas para reducir lo que se imprime en pantalla.

-}
------------------------------------------------------------
--TRABAJOS CON FUNCIONES Y DERIVADAS

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
          Sen Op|                           --sen(x)
          Cos Op|                           --cos(x)
          Tng Op|                           --tng(x)
          Entero Double|                    --k
          Senh Op|                          --senohiperbolico
          Cosh Op|                          --Coseno hiperbolico
          Iden                              --identidad

instance Show Op where
  show (Sum f1 f2) = (show f1)++"+"++(show f2)
  show (Resta f1 f2) = (show f1)++"-"++(show f2)
  show (Multi f1 f2) = (show f1)++"*"++(show f2)
  show (Divi f1 f2) = (show f1)++"/"++(show f2)
  show (Expo f1) = "e^"++ "(" ++ (show f1) ++ ")"
  show (Logar f1) = "Log "++"("++(show f1)++")"
  show (Superexpo f1 f2) = (show f1)++"^"++"("++(show f2)++")"
  show (Paren op) = "("++(show op)++")"
  show (Constant n f) = (show n)++"*"++(show f)
  show (Composicion f1 f2) = (show f1)++"º"++(show f2)
  show (Eleva k f) = (show f)++"^"++(show k)
  show (Sen f) = "Sen("++(show f)++")"
  show (Cos f) = "Cos("++(show f)++")"
  show (Tng f)  = "Tng("++(show f)++")"
  show (Entero n)  = if n == 1 then "" else if n <0 then "("++show n++")" else show n 
  show (Senh f) = "Senh("++(show f)++")"
  show (Cosh f) = "Cosh("++(show f)++")"
  show Iden = "x"


deriva :: Op -> Op
deriva (Paren f) = Paren (deriva f)
deriva (Sum f1 f2) = Sum (deriva f1) (deriva f2)
deriva (Resta f1 f2) = Resta (deriva f1) (deriva f2)
deriva (Multi (f1) (f2)) = Sum (Multi (deriva f1) (f2)) (Multi (f1) (deriva f2))
deriva (Divi f1 f2) = Divi (Resta (Multi (deriva f1) f2) (Multi f1 (deriva f2))) (Eleva 2 f2)
deriva (Expo f) = Multi (Expo f) (deriva f)
deriva (Logar f) = Divi (deriva f) (f)
deriva (Superexpo f1 f2) = undefined
deriva (Constant k f) = Constant k (deriva f)
deriva (Eleva k f) = Multi (Constant k (Eleva (k-1) f)) (deriva f)
deriva (Sen f) = Multi (Cos f) (deriva f)
deriva (Cos f) = Multi (Constant (-1) (Sen f)) (deriva f)
deriva (Tng f) = Multi (Sum (Entero 1) (Eleva 2 (Tng f))) (deriva f)
deriva (Entero _) = Entero 0
deriva (Senh f) = Multi (Cosh f) (deriva f)
deriva (Cosh f) = Multi (Senh f) (deriva f)
deriva Iden = Entero 1


derSup :: Int -> Op -> Op
derSup 0 op = op
derSup n op = derSup (n-1) (deriva op)



evalua :: Double -> Op -> Double  --Evalua un valor en una funcion
evalua a (Paren f) = evalua a f
evalua a (Sum f1 f2) = (evalua a f1)+(evalua a f2)
evalua a (Resta f1 f2) = (evalua a f1)-(evalua a f2)
evalua a (Multi (f1) (f2)) = (evalua a f1)*(evalua a f2)
evalua a (Divi f1 f2) = (evalua a f1)/(evalua a f2)
evalua a (Expo f) = exp(a)
evalua a (Logar f) =log a
evalua a (Superexpo f1 f2) = (evalua a f1)**(evalua a f2)
evalua a (Constant k f) = k*a
evalua a (Eleva k f) =a**k
evalua a (Sen f) = sin a
evalua a (Cos f) =cos a
evalua a (Tng f) = tan a
evalua a (Entero k) = k
evalua a (Senh f) =sinh a
evalua a (Cosh f) =cosh a 
evalua a Iden = a


---------------------------------------------------------------------------------
--TRABAJO CON POLINOMIOS

type Pol = [(Double, Double)]  --   (Exponente,coeficiente)


polAOp :: Pol -> Op
polAOp [x] = Constant (snd x) (Eleva (fst x) Iden)
polAOp (x:xs) =Sum (Constant (snd x) (Eleva (fst x) Iden)) (polAOp xs) 


opAPol :: Op -> Pol
opAPol (Constant k (Eleva n Iden)) = [(n,k)]
opAPol (Sum (Constant k (Eleva n Iden)) (v)) = (n,k) : opAPol v

-----------------------------------------------------------------------------------
--POLINOMIO DE TAYLOR

coefTaylor :: Int -> Double -> Op -> Pol  --COEFICIENTES TAYLORgradoPol,  sobreDonde,   funcion
coefTaylor 0 a fun = [(0,evalua a fun)]
coefTaylor n a fun = (0,(evalua a fun)):[(fromIntegral i,(evalua a (derSup i fun))/(fromIntegral(product[1..i]))) | i<-[1..n]]

coefLaurin :: Int -> Op -> Pol
coefLaurin n f = coefTaylor n 0 f

listaTaylor :: Double -> Pol -> [Op]   --Esto da [k*(x-a)^j]
listaTaylor a [] = []
listaTaylor a (x:xs) = Constant (snd x) (Eleva (fst x) (Resta Iden (Entero a))) : listaTaylor a xs

arreglaPol :: [Op] -> Op   --Toma los terminos y los suma
arreglaPol [x] = x
arreglaPol (x:xs) = Sum x (arreglaPol xs)


taylor :: Int -> Double -> Op -> Op      --DA LA SERIE DE TAYLOR
taylor n a op = arreglaPol (listaTaylor a (coefTaylor n a op))

mcLaurin :: Int -> Op -> Op                    --DA LA SERIE DE MCLAURIN
mcLaurin n op = taylor n 0 op



----------------------------------------------------
--INTEGRALES

--barrow :: Op -> Double -> Double -> Double
--barrow f a b = (evalua b (integral f)) - (evalua a (integral a)) 


integra :: Op -> Op
integra (Paren f) = Paren (integra f)
integra (Sum f g) = Sum (integra f) (integra g)
integra (Resta f g) = Resta (integra f) (integra g)
integra (Multi f g) = undefined
integra (Divi f g) = undefined
integra (Expo f) = undefined
integra (Logar f) = undefined
integra (Constant k f) = undefined
integra (Composicion f g) = undefined
integra (Eleva k f) = undefined
integra (Sen f) = undefined
integra(Cos f) = undefined
integra (Tng f) = undefined
integra (Entero _) = undefined
integra (Senh f) = undefined
integra (Cosh f) = undefined
integra Iden = undefined
