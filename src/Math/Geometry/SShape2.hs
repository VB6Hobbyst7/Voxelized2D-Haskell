module Math.Geometry.SShape2 where

import Math.Linear.Vec
import TypeClass.Vector
import Common

type DenFun a = (Floating a) => Vec2 a -> IO a

intersection :: (Floating a, Ord a) => DenFun a -> DenFun a -> DenFun a
intersection a b point = do
  da <- a point
  db <- b point
  pure $ max da db

union :: (Floating a, Ord a) => DenFun a -> DenFun a -> DenFun a
union a b point = do
  da <- a point
  db <- b point
  pure $ min da db

difference :: (Floating a, Ord a) => DenFun a -> DenFun a -> DenFun a
difference a b point = do
  da <- a point
  db <- b point
  pure $ max da (-db)


circle2 :: (Floating a) => Vec2 a -> a -> DenFun a
circle2 center rad point =
  let d = point |-| center
  in
    pure $ (d `dot` d) - rad * rad


halfPlane2Left :: (Floating a) => a -> DenFun a
halfPlane2Left _x point =
  pure $ point.>x - _x

halfPlane2Right :: (Floating a) => a -> DenFun a
halfPlane2Right _x point =
  pure $ _x - point.>x


halfPlane2Lower :: (Floating a) => a -> DenFun a
halfPlane2Lower _y point =
  pure $ point.>y - _y

halfPlane2Upper :: (Floating a) => a -> DenFun a
halfPlane2Upper _y point =
  pure $ _y - point.>y

rectangle2 :: (Floating a, Ord a) => Vec2 a -> Vec2 a -> DenFun a
rectangle2 center extent =
  halfPlane2Right (center.>x - extent.>x) `intersection`
   halfPlane2Left (center.>x + extent.>x) `intersection`
    halfPlane2Upper (center.>y - extent.>y) `intersection`
     halfPlane2Lower (center.>y + extent.>y)
