import Data.List
-- Exercicios de sala:

fold :: (t -> u -> u) -> u -> [t] -> u
fold f s [] = s 
fold f s (a:as) = f a (fold f s as)

filtro :: (t -> Bool) -> [t] -> [t]
filtro p [] = []
filtro p (a:as) | p a = a: filtro p as 
                | otherwise = filtro p as

f :: [Int] -> [Int]
f lista = map sqr lista
    where sqr a = a * a 

{-
f' :: [Int] -> [Int]
f'

-}

multiply :: Int -> Int -> Int
multiply a b = a * b 

-- Exemplo de aplicação parcial de função
doubleList :: [Int] -> [Int]
doubleList = map (multiply 2)
{-
  Aqui map e multiply tem mais de um parametro, 
  mas eles só usam um, esperando que a função 
  mande o segundo parametro;
-}

maiorQ :: Int -> Int -> Int
maiorQ a b | a > b = a
        | a < b = b
        | a == b = a

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d = maiorQ (maiorQ a b) (maiorQ c d)


converterNotaParaMencao :: Float -> String
converterNotaParaMencao x | x == 0 = "SR"
    | (x >= 0.1) && (x <= 2.9 ) = "II"
    | (x >= 3.0) && (x <= 4.9 ) = "MI"
    | (x >= 5.0) && (x <= 6.9 ) = "MM"
    | (x >= 7) && (x <= 8.9 ) = "MS"
    | (x >= 9) && (x <= 10 ) = "SS"

isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente (a:[]) = True 
isDecrescente (a:b:[]) = a > b
isDecrescente (a:b:bs) = (a > b) && isDecrescente (b:bs)


-- Questão 4 e suas funções ____________________________________________

comparador :: String -> String -> Int
comparador a b | a == b = 1
  | a /= b = 0

contador :: [String] -> String -> Int
contador [] _ = 0
contador (a:[]) b = comparador a b
contador (a:as) b = (comparador a b) + (contador as b) 

novaLista :: [String] -> String -> [String]
novaLista [] _ = []
novaLista (a:as) b | as == [] && a == b = []
  | as == [] && a /= b = [a]
  | as /= [] && a == b = novaLista as b 
  | as /= [] && a /= b = a:(novaLista as b)

histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma (a:[]) = [(a,(comparador a a))] 
histograma (a:as) = (a,1+(contador as a)):(histograma (novaLista as a) )
-- Questão 5____________________________________________________

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ []  = []
myZipWith funcao (a:as) (b:bs) 
  | (null as) || (null bs) = (funcao a b):[]
  | otherwise = (funcao a b):(myZipWith funcao as bs)

-- Questao 6____________________________________________________

mediaNotas :: Float -> Float -> Float
mediaNotas a b = (a+b)/2.0

mediaNotasTotal :: (Float -> Float -> Float) -> [(String,Float,Float)] -> [(String,Float)]
mediaNotasTotal _ [] = []
mediaNotasTotal f ((al,n1,n2):ls) 
  | (null ls) = [(al,(f n1 n2))]
  | not (null ls) = (al,(f n1 n2)):(mediaNotasTotal f ls)

aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia [] = []
aprovadosOrdemDeMedia ((aluno,nota1,nota2):lista)
  | (mediaNotas nota1 nota2) < 5 = sortBy (\(_,a) (_,b) -> compare a b) (aprovadosOrdemDeMedia lista)
  | (mediaNotas nota1 nota2) >= 5 = sortBy (\(_,a) (_,b) -> compare a b) ((aluno,(mediaNotas nota1 nota2) ):(aprovadosOrdemDeMedia lista))

-- Questao 7______________________________________________________