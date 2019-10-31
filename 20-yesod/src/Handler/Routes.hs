{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Routes where

import Import

getHomeR :: Handler String
getHomeR = pure "foo"

postHomeR :: Handler String
postHomeR = pure "foo"

getPathR :: Handler String
getPathR = pure "foo"

getPathPieceR :: Int -> Handler String
getPathPieceR i = pure (show i)

getMultiPathPieceR :: Int -> Int -> Int -> Handler String
getMultiPathPieceR a b c = pure (show (a + b + c))

getFirstR :: Handler String
getFirstR = pure "first"

getAttributeR :: Handler String
getAttributeR = pure "attribute route"

getAfterFirstR :: Int -> Handler String
getAfterFirstR i = pure (show i)

getRoute1R :: Handler String
getRoute1R = pure "route 1 r"

getRoute2R :: Handler String
getRoute2R = pure "route 2 r"

getFinalR :: Handler String
getFinalR = pure "Final R"

getInnerRouteR :: Handler String
getInnerRouteR = pure "inner route"

getSubRouteR :: Handler String
getSubRouteR = pure "sub route"

getEndRouteR :: Handler String
getEndRouteR = pure "end route"
