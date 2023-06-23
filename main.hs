import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Debug.Trace(trace)

import System.Process
import System.Info

data Ação = ColocarBomba | Agir | Mover Direcao | NO_OP | Sair
    deriving (Show, Eq)

data Codigo = Acabou | Saiu deriving (Show, Eq)

data DadosDoJogador = DadosDoJogador Int (Int,Int) String
data Item = Grama | Pres PresenteT | Bomba | Jogador Int | Parede | Pedra 
    deriving (Show,Eq,Ord)
data Direcao = N | S | W | E 
    deriving (Show,Eq,Read)
data PresenteT = Patins | Arremesso 
    deriving (Show,Eq,Ord)
data Partida = Jogando | Perdeu | Ganhou 
    deriving (Show, Eq)

type Tabuleiro   = [Linha]   -- [[[Item]]]
type Linha       = [Célula]  --  [[Item]]
type Célula      = [Item]    -- apenas 4 Item permitidos 
type JogadorV    = (Int, Coordenada, Direcao, Capacidades)
type Coordenada  = (Int, Int)
type Capacidades = ((Item , Int),(Item, Int),(Item, Int)) -- Ordem das capacidades é ((Pres Arremesso, Int), (Bomba, Int), (Pres Patins, Int))
type EstadoJogo = (Tabuleiro, [JogadorV], Partida)

-- Mapeamento de teclas para ações de cada um dos possíveis jogadores.
-- Cada linha corresponde a um jogador.
keyMaps = [(1,[('e',colocarBomba),('r',Agir),('a', Mover W),('s', Mover S),('d',Mover E),('w', Mover N),('Q', Sair)]),
           (2,[('o',colocarBomba),('p',Agir),('j', Mover W),('k', Mover S),('l',Mover E),('i', Mover N),('Q', Sair)])]

-- Dado uma tecla e um mapa, retorna o jogador e a ação a ser executada para ele.
-- Se a tecla é inválida, retorna Nothing.
mapKey :: Char -> [(Int, [(Char, Ação)])] -> Maybe (Int, Ação)
mapKey c []     = Nothing
mapKey c ((j,as):jas) = case mapKey' c as of Nothing -> mapKey c jas
                                             Just a  -> Just (j,a)
    where mapKey' c [] = Nothing
          mapKey' c ((c',a):ms)
            | c == c'   = Just a
            | otherwise = mapKey' c ms

-- Retorna IO id do jogador e ação a ser executada.
-- js é a lista dos jogadores ainda no jogo.
pegaMov :: [Int] -> IO (Maybe (Int,Ação))
pegaMov js = do
        movChar <- getChar
        let mapped = mapKey movChar keyMaps    -- Observe a ausência do `in` quando estamos dentro de um bloco IO.
        case mapped of Nothing     -> return Nothing
                       Just (j,a)  -> if j `elem` js then return mapped
                                                     else return Nothing


tabuleiroExemplo = [[[Parede], [Parede], [Parede], [Parede], [Parede]], [[Parede],[Bomba,Grama],[Grama],[Pres Arremesso,Grama],[Parede]], [[Parede],[Grama],[Jogador 1,Grama],[Pres Patins,Grama],[Parede]], [[Parede],[Grama],[Pres Arremesso, Grama],[Grama],[Parede]], [[Parede], [Parede], [Parede], [Parede], [Parede]]] 

tabuleiro8x8 =  [[[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra]],[[Pedra],[Grama],[Pres Patins,Grama],[Pres Arremesso,Grama],[Bomba,Grama],[Grama],[Pres Patins,Grama],[Pedra]],[[Pedra],[Grama],[Jogador 1,Grama],[Pres Arremesso,Grama],[Pres Patins,Grama],[Bomba,Grama],[Pres Patins,Grama],[Pedra]],[[Pedra],[Grama],[Pres Patins,Grama],[Pres Arremesso,Grama],[Pres Patins,Grama],[Grama],[Pres Patins,Grama],[Pedra]],[[Pedra],[Grama],[Grama],[Pres Arremesso,Grama],[Pres Patins,Grama],[Bomba,Grama],[Pres Patins,Grama],[Pedra]],[[Pedra],[Grama],[Pres Patins,Grama],[Pres Arremesso,Grama],[Pres Patins,Grama],[Grama],[Pres Patins,Grama],[Pedra]],[[Pedra],[Grama],[Grama],[Pres Arremesso,Grama],[Pres Patins,Grama],[Bomba,Grama],[Pres Patins,Grama],[Pedra]],[[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra]]]

dadosDoJogadorExemplo = [jogadorEx]
dadosDoJogador8x8 = [jogador8x8]

jogadorEx = (1,(2,2),N,((Pres Arremesso,0),(Bomba,0),(Pres Patins,0)))
jogador8x8 = (1,(2,2),N,((Pres Arremesso,0),(Bomba,0),(Pres Patins,0)))
 
-- >>> tab = [[[Grama], [Grama], [Grama], [Grama], [Grama]], [[Grama], [Grama], [Grama], [Grama], [Grama]], [[Grama], [Grama], [Grama], [Grama], [Grama]],[[Grama], [Grama], [Grama], [Grama], [Grama]],[[Grama], [Grama], [Grama], [Grama], [Jogador 1, Grama]]]
-- >>>jogador1 = (1,(4,4),N,((Pres Arremesso,2),(Bomba,0),(Pres Patins,1)))
-- >>> a = pegaCelula tab (calculaCoordenadaBomba N jogador1)
-- >>> celBomba =  Bomba : a
-- >>> novoTabBomba = addJogadorCoord tab celBomba (calculaCoordenadaBomba N jogador1) 
-- >>>jogador1
-- >>> novoTabBomba
-- (1,(4,4),N,((Pres Arremesso,2),(Bomba,0),(Pres Patins,1)))
-- [[[Grama],[Grama],[Grama],[Grama],[Grama]],[[Grama],[Grama],[Grama],[Grama],[Grama]],[[Grama],[Grama],[Grama],[Grama],[Bomba,Grama]],[[Grama],[Grama],[Grama],[Grama],[Grama]],[[Grama],[Grama],[Grama],[Grama],[Jogador 1,Grama]]]
-- >>> (pegaCelula tab (calculaCoordenadaBomba W jogador1))
-- [Grama]

-- Funcoes Auxiliares:
éJogador :: Item -> Bool 
éJogador (Jogador _ ) = True 
éJogador _            = False 

éPresente :: Item -> Bool 
éPresente (Pres _) = True 
éPresente _        = False

éBuraco :: Célula -> Bool 
éBuraco [] = True 
éBuraco _  = False

éBomba :: Item -> Bool 
éBomba Bomba = True
éBomba _     = False

éGrama :: Célula -> Bool 
éGrama (Grama:ls) = True 
éGrama _          = False

pegaVal :: (Read a) => String -> IO a
pegaVal msg = do
    putStr msg 
    line <- getLine
    return (read line)

---------------------------------------------------------------------------------------------------
-- Manipulacao Movimento:

buscaItemCelula :: Célula -> Item -> Bool
buscaItemCelula [] _ = False
buscaItemCelula (c:cs) item 
    | item == c = True 
    | otherwise = buscaItemCelula cs item

buscaItemLinha :: Linha -> Item -> Bool
buscaItemLinha [] _ = False
buscaItemLinha (l:ls) item
    | buscaItemCelula l item = True
    | otherwise = buscaItemLinha ls item

calculaCoordenada :: Coordenada -> Direcao -> Coordenada
calculaCoordenada (x, y) N = (x, y - 1)
calculaCoordenada (x, y) S = (x, y + 1)
calculaCoordenada (x, y) W = (x - 1, y)
calculaCoordenada (x, y) E = (x + 1, y)

pegaLinha :: Tabuleiro -> Int -> Linha
pegaLinha (l:ls) 0 = l
pegaLinha (_:ls) y = pegaLinha ls (y-1)

pegaColuna :: Linha -> Int -> Célula 
pegaColuna (c:cs) 0 = c
pegaColuna (_:cs) x = pegaColuna cs (x-1)

pegaCelula :: Tabuleiro -> Coordenada -> Célula
pegaCelula tab (x,y) = pegaColuna (pegaLinha tab y) x

addColuna :: Linha -> Int -> Célula -> Linha
addColuna (_:cs) 0 nc = nc : cs
addColuna (c:cs) x nc = c : addColuna cs (x-1) nc

addLinha :: Tabuleiro -> Int -> Linha -> Tabuleiro
addLinha (_:ls) 0 nc  = nc : ls
addLinha (l:ls) y nc  = l : addLinha ls (y-1) nc

addCelulaLinha :: Linha -> Célula -> Int -> Linha
addCelulaLinha [] _ _ = []
addCelulaLinha (l:ls) c 0 = c : ls
addCelulaLinha (l:ls) c x = l : addCelulaLinha ls c (x - 1)

-- addJogadorCelula :: Célula -> JogadorV -> Célula
-- addJogadorCelula c j
--     | head c == Grama = Jogador id : c
--     | head c == Pres Arremesso || head c == Pres Patins = Jogador id : tail c
--     | head c == Bomba = Jogador id : tail c
--     | otherwise = c
--     where   id = pegaId j

---------------------------------------------------------------------------------------------------
-- Manipulacao Jogador:

pegaX :: Coordenada -> Int
pegaX (x,_) = x
pegaY :: Coordenada -> Int
pegaY (_,y) = y

pegaItem :: JogadorV -> Tabuleiro -> [Linha]
pegaItem jogador tab = [x | x <- tab, buscaItemLinha x (Jogador (pegaId jogador))]

pegaId :: JogadorV -> Int
pegaId (id, _coord, _dir, _capacidades) = id

pegaCoordenada :: JogadorV -> (Int,Int)
pegaCoordenada (_id, coord, _dir, _capacidades) = coord

pegaDirecao :: JogadorV -> Direcao
pegaDirecao (_id, _coord, dir, _capacidades) = dir

alteraCoordenada :: JogadorV -> (Int,Int) -> JogadorV
alteraCoordenada (id, _coord, dir, capacidades) coord = (id, coord, dir, capacidades)

alteraDirecao :: JogadorV -> Direcao -> JogadorV
alteraDirecao (id, coord, _dir, capacidades) dir = (id, coord, dir, capacidades)

addJogadorCelula :: Célula -> JogadorV -> Célula
addJogadorCelula [] _ = []
addJogadorCelula c j = Jogador (pegaId j) : [cel | cel <- c]

addJogadorCoord :: Tabuleiro -> Célula -> Coordenada -> Tabuleiro
addJogadorCoord [] _ _ = []
addJogadorCoord (t:ts) c coord@(x, 0) = addCelulaLinha t c (pegaX coord) : ts
addJogadorCoord (t:ts) c (x, y) = t : addJogadorCoord ts c (x, y - 1)

rmCelulaLinha :: Linha -> Célula -> Int -> Linha
rmCelulaLinha [] _ _ = []
rmCelulaLinha (l:ls) c 0 = c : ls
rmCelulaLinha (l:ls) c x = l : rmCelulaLinha ls c (x - 1)

rmJogadorCoord :: Tabuleiro -> Célula -> Coordenada -> Tabuleiro
rmJogadorCoord [] _ _ = []
rmJogadorCoord (t:ts) c coord@(x, 0) = rmCelulaLinha t c (pegaX coord) : ts
rmJogadorCoord (t:ts) c (x, y) = t : rmJogadorCoord ts c (x, y - 1)

rmJogadorCelula :: Célula -> JogadorV -> Célula
rmJogadorCelula [] _ = []
rmJogadorCelula c j = [cel | cel <- c, cel /= Jogador (pegaId j)]

---------------------------------------------------------------------------------------------------
-- Manipulacao Presente:

coletaItem :: [JogadorV] -> Int -> Item -> [JogadorV]
coletaItem [] _ _ = []
coletaItem (l:ls) id item 
    | pegaId l == id = coletaItem' l item: ls
    | otherwise = l: coletaItem ls id item

coletaItem' :: JogadorV -> Item -> JogadorV
coletaItem' t@(_id, _coord, _dir, ((x, a),(z, b),(y, c))) item
    | x /= Pres Arremesso && y /= Bomba && z /= Pres Patins = error "Padrao esperado -> ((Pres Arremesso, Int), (Bomba, Int), (Pres Patins, Int))"
    | item == Pres Arremesso = (_id, _coord, _dir, ((x, a+1),(Bomba, b),(y, c)))
    | item == Bomba = (_id, _coord, _dir, ((x, a),(Bomba, b+1),(y, c)))
    | item == Pres Patins = (_id, _coord, _dir, ((x, a),(Bomba, b),(y, c+1)))
    | otherwise = t

rmPresenteCelula :: Célula -> Item -> Célula
rmPresenteCelula c (Pres Arremesso) = [cel | cel <- c, cel /= Pres Arremesso]
rmPresenteCelula c (Pres Patins) = [cel | cel <- c, cel /= Pres Patins]
rmPresenteCelula c _ = c

---------------------------------------------------------------------------------------------------
-- Manipulacao Bomba:

temArremesso :: JogadorV -> Bool 
temArremesso (_, _, _, ((Pres Arremesso, 0),(_, _), (_, _))) = False 
temArremesso (_, _, _, ((Pres Arremesso, qtde),(_, _), (_, _))) = True
temArremesso (_, _, _, _) = False

pegaQteArremesso :: JogadorV -> Int 
pegaQteArremesso (_, _, _, ((Pres Arremesso, qtde),(_, _), (_, _))) = qtde
pegaQteArremesso (_, _, _, _) = error "Padrao Capacidades ((Pres Arremesso, Int),(Bomba, Int),(Pres Patins, Int))"

rmBombaCelula :: Célula -> Item -> Célula
rmBombaCelula c Bomba = [cel | cel <- c, cel /= Bomba]
rmBombaCelula c _ = c

-- leArremesso ::  IO (Direcao)
-- leArremesso = do
--     d <- pegaVal "Direcao desejada para chutar a bomba: "
--     return d

leArremesso ::  IO (Direcao)
leArremesso = do
    d <- pegaVal "Direcao desejada para chutar a bomba: "
    return d


ehArremessoValido :: Direcao -> Célula -> Bool 
ehArremessoValido dir celDest
    | head celDest == Grama || head celDest == Pres Arremesso || head celDest == Pres Patins = True 
    | otherwise = False 

calculaCoordenadaBomba :: Direcao -> JogadorV -> Coordenada
calculaCoordenadaBomba dir j@(idJog, cO@(x,y), d, ((pa, qtdePA),(b, qtdeB),(pp, qtdePP)))
    | dir == N && ( y > (y - qtdePA) && (y - qtdePA) >= 1 ) = (x, y - qtdePA)
    | dir == S && ( y < (y + qtdePA) && (y + qtdePA) <= 6 ) = (x, y + qtdePA)
    | dir == W && ( x > (x - qtdePA) && (x - qtdePA) >= 1 ) = (x - qtdePA, y)
    | dir == E && ( x < (x + qtdePA) && (x + qtdePA) <= 6 ) = (x + qtdePA, y)
    | otherwise = cO
    
addBombaCelula :: Célula -> Item -> Célula
addBombaCelula [] _ = []
addBombaCelula c Bomba = Bomba : [cel | cel <- c]
addBombaCelula c _ = [cel | cel <- c]
---------------------------------------------------------------------------------------------------

testaFimDeJogo :: (Tabuleiro, [JogadorV]) -> Bool 
testaFimDeJogo (_, jogs) = null jogs

---------------------------------------------------------------------------------------------------
-- Tabuleiro:

impressaoJogadores :: [JogadorV] -> IO ()
impressaoJogadores [] = return ()
impressaoJogadores ((id, _, orient, ((_, p), (_, f), (_, a))):js) = do
    putStr $ "   " ++ show id ++ "    |    " ++ show orient ++ "    |   " ++ show p ++ "       |  " ++ show f ++ "    |   " ++ show a ++ "\n"
    impressaoJogadores js

impressaoTabuleiro :: Tabuleiro -> IO ()
impressaoTabuleiro [] = return ()
impressaoTabuleiro (l:ls) = do
    impressaoTabuleiro' l
    putStr "\n"
    impressaoTabuleiro ls

impressaoTabuleiro' :: Linha -> IO ()
impressaoTabuleiro' [] = return ()
impressaoTabuleiro' (c:cs) = do
    putStr $ formatacaoTabuleiro $ c
    putStr "\t"
    impressaoTabuleiro' cs

formatacaoTabuleiro :: Célula -> String
formatacaoTabuleiro celula
    | null celula = "Buraco      "
    | c == Grama || c == Bomba || c == Pedra = (show c) ++ "       "
    | c == Parede = (show c) ++ "      "
    | éJogador c = (show c) ++ "    "
    | c == Pres Arremesso = (show c) ++ " "
    | c == Pres Patins = (show c) ++ " "
    where c = head celula

main :: IO ()
main = do
        (id, cod) <- actionLoop i0
        case cod of
            Saiu -> putStr "Fim de jogo, pois escolheu sair!\n\n"
            Acabou -> putStr $ "Fim de jogo. Vencedor = " ++ show id ++ "!\n\n"
        return ()
    where i0 = (tabuleiro8x8, dadosDoJogador8x8)

actionLoop :: (Tabuleiro, [JogadorV]) -> IO (Int, Codigo)
actionLoop ins@(t, js@((id, _, _, _):j)) = do
    system $ clearPrompt
    if testaFimDeJogo ins then return (id, Acabou) else do
        putStr "Jogador | Direcao | Arremesso | Bomba | Pres Patins\n"
        impressaoJogadores js
        putStr "\n"
        impressaoTabuleiro t
        
        let ids = [pegaId i | i <- js]
        move <- pegaMov ids
        let (j,op) = fromMaybe (-1,NO_OP) move
        print $ "(Jogador,Ação)" ++ show (j,op)
        case op of
            ColocarBomba   -> actionLoop $ colocarBomba t js j
            Agir           -> actionLoop $ agir t js j
            Mover d        -> actionLoop $ movimento t js j d
            NO_OP          -> actionLoop (t,js)
            Sair           -> return (j, Saiu)

clearPrompt :: String
clearPrompt
    | os == "darwin" || os == "linux" || os == "linux-android" = "clear" -- $uname no shell
    | otherwise = "cls"


-- Descobre se alguma ação é possível para o jogador e executa.
agir :: Tabuleiro -> [JogadorV] -> Int -> (Tabuleiro, [JogadorV])
agir t js j = (t,js)

-- Verifica se é possível colocar a boma e coloca.
colocarBomba :: Tabuleiro -> [JogadorV] -> Int -> (Tabuleiro, [JogadorV])
colocarBomba t js i = (t,js)

movimento :: Tabuleiro -> [JogadorV] -> Int -> Direcao -> (Tabuleiro,[JogadorV])
movimento tab jogadores id dir
    | éGrama celDestino = (novoTabuleiro, njs) -- Jogador movimenta sobre a Grama, muda sua Coordenada e Direcao.
    | éBuraco celDestino = (novoTabBuraco, njs) -- <<Em desenvolvimento>> Jogador é tirado da celula pois caiu no buraco
    | Pedra == head celDestino || Parede == head celDestino || éJogador itemAtual = (tab, jogadores)
    | éPresente (head celDestino) = (novoTabPres, coletaItem njs id itemAtual) -- Presente é removido e jogador move para a celula.
    | éBomba (head celDestino) && temArremesso jogador = (novoTabBomba, coletaItem njs id itemAtual)
        -- if ehArremessoValido dirInputada celDoChute
        --     then (novoTabBomba, coletaItem njs id itemAtual)
        -- else (novoTabBomba', coletaItem njs id itemAtual)
    -- | éBomba (head celDestino) && temArremesso jogador = (novoTabBomba, coletaItem njs id itemAtual) -- Bomba é arremessada pelo jogador de acordo com a capacidade e capacidade é decrementada, apos arremesso.
    | otherwise = (tab, jogadores) -- Não movimenta
    where  
        itemAtual         = head celDestino
        jogador           = head [x | x <- jogadores, pegaId x == id]
        coordOrigem       = pegaCoordenada jogador
        novaCoord         = calculaCoordenada coordOrigem dir
        celOrigem         = pegaCelula tab coordOrigem
        celDestino        = pegaCelula tab novaCoord
        novoJogador'      = alteraCoordenada jogador novaCoord
        novoJogador       = alteraDirecao novoJogador' dir
        njs               = novoJogador : [x | x <- jogadores, pegaId x /= id]

        removeJCelula     = rmJogadorCelula (pegaCelula tab (pegaCoordenada jogador)) jogador
        celula            = addJogadorCelula (pegaCelula tab (pegaCoordenada novoJogador)) novoJogador
        novoTabuleiro'    = addJogadorCoord tab removeJCelula (pegaCoordenada jogador) 
        novoTabuleiro     = addJogadorCoord novoTabuleiro' celula (pegaCoordenada novoJogador) 

        --Tratamento para a célula Buraco: 
        novaCelOrigem     = tail celOrigem
        novaCelDestino    = head celOrigem : celDestino
        linhaOrigemBuraco = pegaLinha tab (fst coordOrigem)
        novaLinhaOBuraco  = addColuna linhaOrigemBuraco (snd coordOrigem) novaCelOrigem
        linhaDBuraco      = pegaLinha tab (pegaY novaCoord)
        novaLinhaDBuraco  = addColuna linhaOrigemBuraco (pegaY novaCoord) novaCelDestino
        novoTabBuraco     = addLinha tab (pegaY coordOrigem) novaLinhaOBuraco
        
        -- Tratamento celula com Presente:
        removePCelula     = rmPresenteCelula (pegaCelula tab novaCoord) (head (pegaCelula novoTabuleiro' novaCoord))
        novoTabPres'      = addJogadorCoord novoTabuleiro' removePCelula (pegaCoordenada novoJogador) 
        celulaPres        = addJogadorCelula (pegaCelula novoTabPres' (pegaCoordenada novoJogador)) novoJogador
        novoTabPres       = addJogadorCoord novoTabPres' celulaPres (pegaCoordenada novoJogador) 

        -- Tratamento celula com Bomba
        removeBCelula     = rmBombaCelula (pegaCelula tab novaCoord) (head (pegaCelula novoTabuleiro' novaCoord))
        novoTabBomba''    = addJogadorCoord novoTabuleiro' removeBCelula (pegaCoordenada novoJogador) 
        celulaBomba       = addJogadorCelula (pegaCelula novoTabBomba'' (pegaCoordenada novoJogador)) novoJogador
        novoTabBomba'     = addJogadorCoord novoTabBomba'' celulaBomba (pegaCoordenada novoJogador)

        coordDestBomba    = calculaCoordenadaBomba E novoJogador -- << adicionar dirEscolhida >>
        -- Exemplo de uso do trace: 
        -- coordDestBomba    = trace ("Coordenada Destino:" ++ show resultado ++ " " ++ show (pegaCoordenada novoJogador) ++ " " ++ show (pegaDirecao novoJogador)) resultado
        --     where resultado = calculaCoordenadaBomba dirEsc novoJogador   
        destinoBomba      = pegaCelula novoTabBomba' coordDestBomba
        novaCelBomba      = addBombaCelula destinoBomba Bomba
     
        novoTabBomba      = addJogadorCoord novoTabBomba' novaCelBomba coordDestBomba 

-- Duvidas:
 -- -> Jogador cai no buraco, como tratar o fim do jogo, dado o retorno da funcao movimento ser (Tab,[JogadorV])?
 -- -> Ao manipular o presente, o jogador consegue ir para posicao dele, mas quando movimenta para outra posicao, o presente esta voltando para a celula antiga. Como excluir o presente de vez da fila?

-- >>>jogador1 = (1,(1,1),N,((Pres Arremesso,1),(Bomba,0),(Pres Patins,1)))
-- >>> coordDest = calculaCoordenadaBomba W jogador1
-- >>> coordDest
-- (1,1)
