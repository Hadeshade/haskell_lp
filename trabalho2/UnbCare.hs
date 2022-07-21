module UnBCare where

import ModeloDados

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
quickSort [] = []
quickSort (a:as) = quickSort [e | e <- as, e < a] ++ [a] ++ quickSort [e | e <- as, e > a]

receituarioValido :: Receituario -> Bool
receituarioValido [] = False
receituarioValido receit = ((quickSort receit ) == receit ) && ((map (snd) receit) == (map (quickSort . snd) receit))

-- Questão 5b ______________________________________________________________________
planoValido :: PlanoMedicamento -> Bool
planoValido [] = False
planoValido planomed = ((quickSort planomed ) == planomed ) && ((map (snd) planomed) == (map (quickSort . snd) planomed))

-- Questão 6 e funções auxiliares ______________________________________________________________________
plantaoValido :: Plantao -> Bool
plantaoValido plant = (quickSort ordenaPorHora == ordenaPorHora) && (map quickSort analisaMedicamentos == analisaMedicamentos) && compraeMedicar
    where
        ordenaPorHora = fst(unzip plant)
        analisaMedicamentos = map (map desmanchacuidado) (snd (unzip plant))
        compraeMedicar = and (map (not . allTheSame) analisaMedicamentos)


desmanchacuidado :: Cuidado -> Medicamento
desmanchacuidado cuid = case cuid of
    Comprar med _ -> med
    Medicar med -> med

allTheSame :: (Eq a) => [a] -> Bool
allTheSame (a:[]) = False
allTheSame list = all (== head list) (tail list)

-- Questão 7 e funções auxiliares: ______________________________________________________________________
geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario ((med,hors):[]) = arrumarMedicamentos (quickSort (zip hors (replicate (length hors) [med])))
geraPlanoReceituario ((med,hors):as) = arrumarMedicamentos (quickSort (zip hors (replicate (length hors) [med]) ++ geraPlanoReceituario as))

arrumarMedicamentos :: PlanoMedicamento -> PlanoMedicamento
arrumarMedicamentos [] = []
arrumarMedicamentos (a:[]) = [a]
arrumarMedicamentos (a:b:as)
 | fst a == fst b = [(fst a,(snd a ++ snd b))] ++ arrumarMedicamentos as
 | otherwise = [a] ++ arrumarMedicamentos (b:as)

-- Questão 8 ______________________________________________________________________
geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = []
geraReceituarioPlano ((horario,medicamentos):[]) = arrumaReceituario(quickSort (zip medicamentos (replicate (length medicamentos) [horario])))
geraReceituarioPlano ((horario,medicamentos):planosMed) = arrumaReceituario(quickSort (zip medicamentos (replicate (length medicamentos) [horario]) ++ geraReceituarioPlano planosMed))

arrumaReceituario :: Receituario -> Receituario
arrumaReceituario [] = []
arrumaReceituario (a:[]) = [a]
arrumaReceituario (a:b:list)
    | fst a == fst b = [(fst a, (snd a ++ snd b))] ++ arrumaReceituario list
    | otherwise = [a] ++ arrumaReceituario (b:list)

-- Questão 9 (A FAZER) ______________________________________________________________________
executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = undefined

-- Questão 10 (A FAZER) ______________________________________________________________________
satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz = undefined

-- Questão 11 (A FAZER) ______________________________________________________________________
plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto = undefined