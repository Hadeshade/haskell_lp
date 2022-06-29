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