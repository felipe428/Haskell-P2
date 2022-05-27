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

checFullREnc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & 
    \case 
          Left err -> error $ unpack err
          Right encoder -> encoder

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()


data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()


fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)


concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
