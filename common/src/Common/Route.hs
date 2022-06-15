{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import Data.Text (Text,unpack)
import Data.Functor.Identity
import Data.Function
import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Produto :: BackendRoute () 
  BackendRoute_Listar :: BackendRoute () 
  BackendRoute_Buscar :: BackendRoute Int

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Produto -> PathSegment "produto" $ unitEncoder mempty
      BackendRoute_Listar  -> PathSegment "listar" $ unitEncoder mempty
      BackendRoute_Buscar  -> PathSegment "buscar" readShowEncoder
      )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

checFullREnc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & 
    \case 
          Left err -> error $ unpack err
          Right encoder -> encoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
