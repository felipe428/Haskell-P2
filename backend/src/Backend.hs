{-# LANGUAGE LambdaCase, GADTs, OverloadedStrings, ScopedTypeVariables #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Common.Api
import Data.Aeson.Text

getConn :: ConnectInfo
getConn  = ConnectInfo "ec2-52-73-184-24.compute-1.amazonaws.com"
                       5432
                       "iwheilsowrvnhp"
                       "6940ae253a8b26275a861f6c569f449f1082c24126f49a73ea4d1fe95d40b427"
                       "d9kt92tmag728f"

migrateProd :: Query
migrateProd = "CREATE TABLE IF NOT EXISTS produto (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"
 
backend :: Backend BackendRoute FrontendRoute
backend = Backend
   { _backend_run = \serve -> do
       dbcon <- connect getConn
       serve $ do
           \case 
                 BackendRoute_Listar :/ () -> method GET $ do
                     res :: [Produto] <- liftIO $ do
                         execute_ dbcon migrateProd
                         query_ dbcon "SELECT * from produto" 
                     modifyResponse $ setResponseStatus 200 "OK"
                     writeLazyText (encodeToLazyText res)
                 BackendRoute_Buscar :/ pid -> method GET $ do
                     res :: [Produto] <- liftIO $ do
                         execute_ dbcon migrateProd
                         query dbcon "SELECT * from produto WHERE id=?" (Only (pid :: Int))
                     if res /= [] then do
                         modifyResponse $ setResponseStatus 200 "OK"
                         writeLazyText (encodeToLazyText (Prelude.head res))
                     else
                         modifyResponse $ setResponseStatus 404 "NOT FOUND"
                 BackendRoute_Produto :/ () -> method POST $ do
                     prod <- A.decode <$> readRequestBody 2000
                     case prod of
                          Just produto -> do
                              liftIO $ do
                                  execute_ dbcon migrateProd
                                  execute dbcon "INSERT INTO produto (nome,valor,qt) VALUES (?,?,?)" 
                                          (produtoNome produto, produtoValor produto, produtoQt produto)
                              modifyResponse $ setResponseStatus 200 "OK"
                          Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                 _ -> return ()
   , _backend_routeEncoder = fullRouteEncoder
   }
