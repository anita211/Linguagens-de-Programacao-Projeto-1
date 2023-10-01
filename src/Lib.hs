module Lib
    ( someFunc, maior4, converterNotaParaMencao, isDecrescente, histograma, myZipWith, aprovadosOrdemDeMedia, somaMatricial, matrizTransposta, multiplicacaoMatricial
    ) where

import Data.List (sortBy)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- 1) (Valor da questão: 1,0 ponto) 
-- Defina uma função que retorne o maior entre quatro inteiros.

maxi :: Int -> Int -> Int
maxi a b | a >= b = a
         | otherwise = b

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d = maxi d (maxi c (maxi a b))

-- 2) (Valor da questão: 1,0 ponto) 
-- Defina uma função que receba uma nota e retorne a menção do aluno.
-- Não se preocupe com a validação do input. A nota sempre será um Número entre 0.0 (inclusive) e 10.0 (inclusive).
-- Considere a seguinte tabela para tradução da menção:
-- De 9 a 10 -> "SS"
-- De 7 a 8.9 -> "MS"
-- De 5 a 6.9 -> "MM"
-- De 3 a 4.9 -> "MI"
-- De 0.1 a 2.9 -> "II"
-- De 0 -> "SR"
converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota | nota == 0.0 = "SR"
                             | (0.1 <= nota) && (nota <= 2.9) = "II"
                             | (3.0 <= nota) && (nota <= 4.9) = "MI"
                             | (5.0 <= nota) && (nota <= 6.9) = "MM"
                             | (7.0 <= nota) && (nota <= 8.9) = "MS"
                             | (9.0 <= nota) && (nota <= 10.0) = "SS"
                             | otherwise = "Nota inválida"

-- 3) (Valor da questão: 1,0 ponto) 
-- defina uma função que retorna um booleano indicando se uma lista de inteiros é decrescente ou não:
isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente [_] = True
isDecrescente (a:as) | a > head as = isDecrescente as
                     | otherwise = False

-- 4) (Valor da questão: 2,0 pontos) 
-- defina uma função que recebe uma lista de strings como entrada e computa uma lista de pares 
-- de (String, Int) representando o histograma de seus elementos:

counter :: Eq t => t -> [t] -> Int
counter _ [] = 0
counter x (a:as) | x == a = 1 + counter x as
                 | otherwise = counter x as

removeRepetidos :: Eq t => [t] -> [t]
removeRepetidos [] = []
removeRepetidos (a:as) | counter a as == 0 = a : removeRepetidos as
                       | otherwise = removeRepetidos as

countList :: Eq t => [t] -> [t] -> [(t,Int)]
countList [] _ = []
countList (e:es) ls = (e, counter e ls) : countList es ls

histograma :: [String] -> [(String, Int)]
histograma xs = countList (removeRepetidos xs) xs

-- 5)(Valor da questão: 1,5 ponto) 
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas, 
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas 
-- listas:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs


-- 6) (Valor da questão: 2,0 ponto) 
-- Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,
-- determinar a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se, e somente se, tal
-- média é maior ou igual a cinco.
-- OBSERVAÇÃO: especificamente para este exercício, você pode importar as funções de ordenaçao de listas (como 'sort' ou 'sortBy') se achar necessário.

aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia xs = sortBy (\(_,a) (_,b) -> compare a b) (aprovados xs)
    where media n1 n2 = (n1+n2)/2
          aprovados db = [(nome, media n1 n2) | (nome, n1, n2) <- db, media n1 n2 >= 5]

-- 7) (Valor da questão: 1,5 ponto, sendo 0.5 ponto para cada letra) 
-- Considere a representação de matrizes como lista de listas em que cada elemento da lista é uma lista 
-- que representa uma linha da matriz. Com base nisso, determine as seguintes funções:
--  a) some duas matrizes
--  b) compute a transposta de duas matrizes 
--  c) compute a multiplicação de duas matrizes
-- OBSERVAÇÃO: considere que os inputs são válidos (ou seja, as matrizes são válidas e as suas dimensões são compatíveis para soma e multiplicação)
somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial [] _ = []
somaMatricial _ [] =[]
somaMatricial (m1:m1s) (m2:m2s) = myZipWith (+) m1 m2 : somaMatricial m1s m2s 

matrizTransposta :: [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta ([]:_) = []
matrizTransposta ms = map head ms : matrizTransposta (map tail ms)

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial [] _ = []
multiplicacaoMatricial (l1:ls1) m2 = linha l1 m2 : multiplicacaoMatricial ls1 m2
    where linha l_m1 mat2 = [ sum (myZipWith (*) l_m1 l2) | l2 <- matrizTransposta mat2]


