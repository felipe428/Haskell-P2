{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Text as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Reflex.Dom.Core
import Data.Maybe
import Text.Read
import Common.Api
import Common.Route
import Data.Text
import Data.Map (Map)
import Data.Aeson

data Pagina = Pagina1 | Pagina2 | Pagina3

getPath :: R BackendRoute -> T.Text
getPath r = renderBackendRoute checFullREnc r

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "P2 - Haskell"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      mainPag
  }

prods :: ( DomBuilder t m
         , Prerender js t m
          ) => m ()
prods = do
  elAttr "main" ("class" =: "container") $ do
        elAttr "div" ("class" =: "spacing") $ do
        elAttr "h1" ("class" =: "title") $ text "Todos os produtos"
        elAttr "div" ("class" =: "spacing") $ do

        elAttr "div" ("class" =: "row") $ do
          elAttr "img" ("src" =: "https://mfresh.s3.amazonaws.com/uploads/product/sku/8242/image/b5d7d51d-3ded-43e0-ac47-5975de6bac09.jpg") blank

          elAttr "div" ("class" =: "column") $ do
            elAttr "span" ("class" =: "text") $ text "Peito de Frango"
            elAttr "span" ("class" =: "text") $ text "Preço: R$ 30,00"

        elAttr "div" ("class" =: "row") $ do
          elAttr "img" ("src" =: "https://scfoods.fbitsstatic.net/img/p/tomate-debora-maduro-para-molho-500g-70892/257510.jpg?w=800&h=800&v=no-change&qs=ignore") blank

          elAttr "div" ("class" =: "column") $ do
            elAttr "span" ("class" =: "text") $ text "Tomate"
            elAttr "span" ("class" =: "text") $ text "Preço: R$ 9,00"

        elAttr "div" ("class" =: "row") $ do
          elAttr "img" ("src" =: "https://cdn.awsli.com.br/800x800/334/334766/produto/18043861/e57164bfb6.jpg") blank

          elAttr "div" ("class" =: "column") $ do
            elAttr "span" ("class" =: "text") $ text "Alface"
            elAttr "span" ("class" =: "text") $ text "Preço: R$ 5,80"

        elAttr "div" ("class" =: "row") $ do
          elAttr "img" ("src" =: "https://espetinhodesucesso.com.br/wp-content/uploads/2017/06/o-que-%C3%A9-carne-maturada.jpg.webp") blank

          elAttr "div" ("class" =: "column") $ do
            elAttr "span" ("class" =: "text") $ text "Carne"
            elAttr "span" ("class" =: "text") $ text "Preço: R$ 50,20"
        
        elAttr "div" ("class" =: "row") $ do
          elAttr "img" ("src" =: "https://static.clubeextra.com.br/img/uploads/1/747/20213747.jpg") blank

          elAttr "div" ("class" =: "column") $ do
            elAttr "span" ("class" =: "text") $ text "Salgadinho"
            elAttr "span" ("class" =: "text") $ text "Preço: R$ 10,00"

        elAttr "div" ("class" =: "row") $ do
          elAttr "img" ("src" =: "https://static.paodeacucar.com/img/uploads/1/643/20247643.jpg") blank

          elAttr "div" ("class" =: "column") $ do
            elAttr "span" ("class" =: "text") $ text "Refrigerante"
            elAttr "span" ("class" =: "text") $ text "Preço: R$ 8,00"

        elAttr "div" ("class" =: "row") $ do
          elAttr "img" ("src" =: "https://static.paodeacucar.com/img/uploads/1/379/649379.png") blank

          elAttr "div" ("class" =: "column") $ do
            elAttr "span" ("class" =: "text") $ text "Pote de açai"
            elAttr "span" ("class" =: "text") $ text "Preço: R$ 12,00"

        elAttr "div" ("class" =: "row") $ do
          elAttr "img" ("src" =: "https://static.clubeextra.com.br/img/uploads/1/733/13536733.jpg") blank

          elAttr "div" ("class" =: "column") $ do
            elAttr "span" ("class" =: "text") $ text "Pote de sorvete"
            elAttr "span" ("class" =: "text") $ text "Preço: R$ 15,00"
                 
form:: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
form = do
  elAttr "main" ("class" =: "container") $ do
    elAttr "div" ("class" =: "spacing") $ do
    elAttr "h1" ("class" =: "title") $ text "Insira os produtos"
    elAttr "div" ("class" =: "spacing") $ do
    
    elAttr "label" ("class" =: "title") (text "Produto:") 
    nome <- inputElement def
    elAttr "label" ("class" =: "title") (text "Valor:") 
    vl <- numberInput
    elAttr "label" ("class" =: "title") (text "Quantidade:") 
    qt <- numberInput

    let prod = fmap (\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
    (submitBtn, _) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current prod) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Produto :/ ()) <$> prodEvt))
    return ()

tabProduto :: DomBuilder t m => Produto -> m ()
tabProduto pr = do 
    el "tr" $ do
        el "td" (text $ T.pack $ show $ produtoId pr)
        el "td" (text $ produtoNome pr)
        el "td" (text $ T.pack $ show $ produtoValor pr)
        el "td" (text $ T.pack $ show $ produtoQt pr)

reqLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
    elAttr "main" ("class" =: "container") $ do
      elAttr "div" ("class" =: "spacing") $ do
      elAttr "h1" ("class" =: "title") $ text "Confira sua lista"
      elAttr "div" ("class" =: "spacing") $ do

        (btn, _) <- el' "button" (text "Listar")
        let click = domEvent Click btn
        prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> click))
        dynP <- foldDyn (\ps d -> case ps of
                            Nothing -> []
                            Just p -> d++p) [] (switchDyn prods)
        el "br" (blank)
        el "br" (blank)
                            
        el "table" $ do
          el "thead" $ do
            el "tr" $ do
              elAttr "th" ("class" =: "tabela_id") (text "Id")
              elAttr "th" ("class" =: "tabela_nome") (text "Nome")
              elAttr "th" ("class" =: "tabela_valor") (text "Valor")
              elAttr "th" ("class" =: "tabela_quantidade") (text "Qt")

          el "tbody" $ do
            dyn_ (fmap sequence (ffor dynP (fmap tabProduto)))

clickLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return $ (const p) <$> (domEvent Click ev)

menuLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
        li1 <- clickLi Pagina1 "Produtos"
        li2 <- clickLi Pagina2 "Listar"
        li3 <- clickLi Pagina3 "Ver Lista"
        return (leftmost [li1, li2, li3])
    holdDyn Pagina1 evs

currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = do
    case p of
         Pagina1 -> prods
         Pagina2 -> form
         Pagina3 -> reqLista

mainPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => m ()
mainPag = do 
    pagina <- el "header" menuLi
    dyn_ $ currPag <$> pagina 

numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number") -- <> "class" =: "myClass")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) (_inputElement_value n)
