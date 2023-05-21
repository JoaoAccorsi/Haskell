import Data.Function (on)
import Data.List (sortBy)

data Pessoa = Pessoa
  { nome :: String,
    idade :: Int,
    endereco :: String,
    cep :: String
  }
  deriving (Show, Eq)

-- Função para receber os dados de uma pessoa
lerPessoa :: IO Pessoa
lerPessoa = do
  putStrLn ("\nInício cadastro pessoa")
  putStrLn "-----------------------------------------------------"
  putStr "Nome: "
  nome <- getLine
  putStr "Idade: "
  idade <- readLn
  putStr "Endereço: "
  endereco <- getLine
  putStr "CEP (somente números): "
  cep <- getLine
  putStrLn ("\nFinal cadastro pessoa")
  putStrLn "-----------------------------------------------------"
  return (Pessoa nome idade endereco cep)

-- Função para ler 10 cadastros
lerCadastros :: IO [Pessoa]
lerCadastros = sequence (replicate 10 lerPessoa)

-- Função para calcular a média de idade
mediaIdade :: [Pessoa] -> Double
mediaIdade pessoas = fromIntegral (sum idades) / fromIntegral (length pessoas)
  where
    idades = map idade pessoas

-- Função para calcular a distância entre dois CEPs
distanciaCEP :: String -> String -> Int
distanciaCEP cep1 cep2 = abs (read cep1 - read cep2)

-- Função para determinar quem mora mais próximo
pessoaMaisProxima :: Pessoa -> [Pessoa] -> Pessoa
pessoaMaisProxima pessoa pessoas = head (sortBy (compare `on` distancia) (filter (/= pessoa) pessoas))
  where
    distancia p = distanciaCEP (cep pessoa) (cep p)

-- Função para exibir o relatório
exibirRelatorio :: [Pessoa] -> IO ()
exibirRelatorio pessoas = do
  putStrLn "*****************************************************\n** RELATÓRIO **\n*****************************************************"
  putStrLn "-----------------------------------------------------"
  putStrLn ("\n* Média de idades\n\n" ++ show (mediaIdade pessoas))
  putStrLn "\n* Pessoas mais próximas\n"
  mapM_ (exibirPessoaMaisProxima pessoas) pessoas

exibirPessoaMaisProxima :: [Pessoa] -> Pessoa -> IO ()
exibirPessoaMaisProxima pessoas pessoa = do
  let maisProxima = pessoaMaisProxima pessoa pessoas
  putStrLn ("-> " ++ nome pessoa ++ " mora mais perto de " ++ nome maisProxima)

-- Função principal
main :: IO ()
main = do
  putStrLn "\nCADASTRE 10 PESSOAS INFORMANDO OS DADOS SOLICITADOS:"
  putStrLn "-----------------------------------------------------"
  cadastros <- lerCadastros
  putStrLn ""
  exibirRelatorio cadastros
