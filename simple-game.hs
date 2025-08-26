-- Importa a biblioteca Gloss, que precisamos para criar a visualização.
import Graphics.Gloss.Interface.Pure.Game

-- | O "Mundo" do nosso programa. 
--   Neste caso, ele só precisa guardar a posição do mouse,
--   que é um par de números de ponto flutuante (Float, Float).
type World = (Float, Float)

-- | O estado inicial do nosso mundo.
--   Começamos com a posição do mouse em (0, 0), o centro da tela.
initialWorld :: World
initialWorld = (0, 0)

-- | A função principal para desenhar.
--   Ela recebe o estado atual do mundo (a posição do mouse)
--   e retorna a imagem (Picture) que será desenhada na tela.
drawWorld :: World -> Picture
drawWorld (mouseX, mouseY) =
  -- `Translate` move a origem (0,0) para a posição (mouseX, mouseY).
  -- Tudo que for desenhado depois será relativo a essa nova posição.
  Translate mouseX mouseY
    -- `Color` muda a cor do que será desenhado a seguir.
    (Color chartreuse
      -- `circleSolid` desenha um círculo preenchido com um raio de 50 pixels.
      (circleSolid 50))

-- | A função para atualizar o mundo.
--   Ela é chamada sempre que um evento acontece.
handleInput :: Event -> World -> World
handleInput event currentPosition =
  -- Usamos "pattern matching" para verificar que tipo de evento recebemos.
  case event of
    -- Se o evento for um movimento do mouse...
    (EventMotion (x, y)) ->
      -- ...nós atualizamos o mundo com a nova posição (x, y) do mouse.
      (x, y)
    
    -- Para qualquer outro tipo de evento (clique, tecla, etc.)...
    _ ->
      -- ...nós simplesmente retornamos a posição atual sem modificá-la.
      currentPosition

-- | A função principal que inicia o programa.
main :: IO ()
main = play
  (InWindow "Círculo Seguidor"   -- Título da janela
            (600, 400)         -- Tamanho da janela (largura, altura)
            (10, 10))          -- Posição inicial da janela na tela
  white                        -- Cor de fundo da janela
  60                           -- Taxa de atualização (frames por segundo)
  initialWorld                 -- O estado inicial do nosso mundo
  drawWorld                    -- A função que desenha o mundo
  handleInput                  -- A função que lida com eventos
  (\_ world -> world)          -- Uma função de "passo" (step), que não usaremos aqui.
                               -- Ela é chamada a cada frame, independentemente de eventos.
                               -- Aqui, ela simplesmente não faz nada.