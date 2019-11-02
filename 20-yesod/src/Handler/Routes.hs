{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Routes where

import Import

-- Normally, we'd use Handler here. However, it's deprecated.
-- https://hackage.haskell.org/package/yesod-core-1.6.16.1/docs/Yesod-Core-Handler.html#t:HandlerT
--
-- so, we've defined our own type alias in `Foundation.hs` to replace it
-- called `HandlerX` and we'll be using it here below.
-- type HandlerX a = HandleFor App a
getHomeR :: HandlerX String
getHomeR = pure "home - get"

postHomeR :: HandlerX String
postHomeR = pure "home - post"

handleAllHttpMethodsR :: HandlerX String
handleAllHttpMethodsR = pure "all http methods handled regardless of method"

getPathR :: HandlerX String
getPathR = pure "path"

getPathPieceR :: Int -> HandlerX String
getPathPieceR i = pure ("path piece " <> show i)

getMultiPathPieceR :: Int -> Int -> Int -> HandlerX String
getMultiPathPieceR a b c = pure ("path pieces: " <> show (a + b + c))

getFirstR :: HandlerX String
getFirstR = pure "first"

getAttributeR :: HandlerX String
getAttributeR = pure "attribute route"

getAfterFirstR :: Int -> HandlerX String
getAfterFirstR i = pure ("after first: " <> show i)

getRoute1R :: HandlerX String
getRoute1R = pure "route 1 r"

getRoute2R :: HandlerX String
getRoute2R = pure "route 2 r"

getFinalR :: HandlerX String
getFinalR = pure "Final R"

getInnerRouteR :: HandlerX String
getInnerRouteR = pure "inner route"

getSubRouteR :: HandlerX String
getSubRouteR = pure "sub route"

getEndRouteR :: HandlerX String
getEndRouteR = pure "end route"

-- See `config/routes` file for corresponding section.
-- Invalid syntax: a route cannot respond to a request (as indicated
-- by 'GET') and be a hierarchical route at the same time (as indicated
-- by the ':').
--
-- Uncomment these lines to see for yourself.
--getHierarchyAndHttpMethod :: HandlerX String
--getHierarchyAndHttpMethod = pure "hierarchy and http method"
--
--getInnerValRouteR :: HandlerX String
--getInnerValRouteR = pure "inner val route"

-- Routes requiring authorization

getAuthorizedByPathR :: HandlerX String
getAuthorizedByPathR = pure "authorized by path"

getAuthedFirstR :: HandlerX String
getAuthedFirstR = pure "first - authorized by top-level attribute"

getAuthedSecondR :: HandlerX String
getAuthedSecondR = pure "second - authorized by top-level attribute"
