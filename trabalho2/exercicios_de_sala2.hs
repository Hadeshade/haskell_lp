-- Modelo de dados:

module Root.Modelo.ModeloDados where

type Medicamento = String

type Quantidade = Int

type Horario = Int

type EstoqueMedicamentos = [(Medicamento, Quantidade)]

type Prescricao = (Medicamento, [Horario])

type Receituario = [Prescricao]

type PlanoMedicamento = [(Horario, [Medicamento])]

type Plantao = [(Horario, [Cuidado])]

data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento

instance Show Cuidado where
  show (Comprar m q) =
    "Comprar "
      ++ Prelude.show q
      ++ " comprimido(s) do medicamento: "
      ++ m
  show (Medicar m) = "Ministrar medicamento: " ++ m

-- Questões trabalho 2:

-- Questão 1 e função auxiliar: ______________________________________________________________________
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med quant [] = [(med,quant)] 
comprarMedicamento med quant (a:as)
 | med == fst a = [(med, snd a + quant)] ++ as
 | percorrerEstoque med (a:as) == True = [a] ++ comprarMedicamento med quant as
 | percorrerEstoque med (a:as) == False = [(med,quant)] ++ [a] ++ as

percorrerEstoque :: Medicamento -> EstoqueMedicamentos -> Bool
percorrerEstoque med (a:as) 
 | med == fst a = True
 | null as = fst a == med 
 | med /= fst a = percorrerEstoque med as

-- Questão 2 ______________________________________________________________________
tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento med (a:as)
 | percorrerEstoque med (a:as) == False = Nothing 
 | percorrerEstoque med (a:as) == True && med == fst a && snd a /= 0 = Just ([(fst a, (snd a) -1)] ++ as)
 | percorrerEstoque med (a:as) == True && med /= fst a =  (tomarMedicamento med as )
 | snd a == 0 = Nothing

-- Questão 3 ______________________________________________________________________
consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento medicamento (a:as)
 | medicamento == fst a = snd a 
 | null as = 0 
 | medicamento /= fst a = consultarMedicamento medicamento as
 | otherwise = 0

-- Questao 4 ______________________________________________________________________
demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos (a:as)
 | null as = [(fst a, length (snd a))] 
 | not (null as) = [(fst a, length (snd a))] ++ demandaMedicamentos as 

-- Questão 5a e funções auxiliares ______________________________________________________________________
receituarioValido :: Receituario -> Bool
receituarioValido [] = False
receituarioValido (a:as)
 | percorrerReceituario (fst a) as == True  = False
 | null as = True  
 | percorrerReceituario (fst a) as == False = receituarioValido as 

quickSort [] = []
quickSort (a:as) = quickSort [e | e <- as, e < a] ++ [a] ++ quickSort [e | e <- as, e > a]

percorrerReceituario :: Medicamento -> Receituario -> Bool
percorrerReceituario med [] = False
percorrerReceituario med (a:as) 
 | med == (fst a) = True
 | null as = (fst a) == med 
 | med /= fst a = percorrerReceituario med as

-- Questão 5b e funções auxiliares ______________________________________________________________________
planoValido :: PlanoMedicamento -> Bool
planoValido [] = False
planoValido (a:as)
 | percorrerPlanoMedicamento (fst a) as == True  = False
 | null as = True  
 | percorrerPlanoMedicamento (fst a) as == False = planoValido as
 | otherwise = False

percorrerPlanoMedicamento :: Horario -> PlanoMedicamento -> Bool
percorrerPlanoMedicamento hor [] = False
percorrerPlanoMedicamento hor (a:as) 
 | hor == (fst a) = True
 | null as = (fst a) == hor 
 | hor /= fst a = percorrerPlanoMedicamento hor as

-- Questão 6 (A FAZER)
plantaoValido :: Plantao -> Bool
plantaoValido = undefined

-- Questão 7 e funções auxiliares: ______________________________________________________________________
geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario ((med,quant):as)
 | null as = arrumarMedicamentos (gerarMedicamentos ((med,quant):as)) 
 | otherwise = arrumarMedicamentos (gerarMedicamentos ((med,quant):as))

arrumarMedicamentos :: PlanoMedicamento -> PlanoMedicamento
arrumarMedicamentos [] = []
arrumarMedicamentos (a:[]) = [a]
arrumarMedicamentos (a:b:as)
 | fst a == fst b = [(fst a,(snd a ++ snd b))] ++ arrumarMedicamentos as
 | otherwise = [a] ++ arrumarMedicamentos (b:as)

gerarMedicamentos :: Receituario -> PlanoMedicamento
gerarMedicamentos [] = []
gerarMedicamentos ((med,hors):as)
 | null as = quickSort (zip hors (replicate (length hors) [med]))
 | not (null as) = quickSort (zip hors (replicate (length hors) [med]) ++ gerarMedicamentos as)

-- Questão 8 (A FAZER) ______________________________________________________________________
geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano = undefined

-- Questão 9 (A FAZER) ______________________________________________________________________
executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = undefined

-- Questão 10 (A FAZER) ______________________________________________________________________
satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz = undefined

-- Questão 11 (A FAZER) ______________________________________________________________________
plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto = undefined