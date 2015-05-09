{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.Map as M

data GetRequest = GetRequest deriving (Eq, Show)
data PostRequest = PostRequest deriving (Eq, Show)

-- RequestType becomes our fake sum type, which is open in nature due to the union of the variants
-- occurring through type classing.
class RequestType a
instance RequestType GetRequest
instance RequestType PostRequest

-- Each operation will require its own wrapper type. This isn't a major limitation, but does require a bit of extra book keeping.
-- In more detail, we can't simply wrap the fake sum type in a single existential type, as the existential black box only
-- permits calling functions that belong to type classes within the existential constraints. Since there are no functions
-- in the fake-sum-type type class, we can't do anything with it. Thus the need to wrap it up with further constraints.
data HTTPReq = forall a. Handleable a => HTTPReq {
   method :: a
  ,path :: String
}

-- Need to make wrapper type part of the fake sum type so that we can make instance definitions for it without
-- violating the type variable constraint in any typeclass definitions.
instance RequestType HTTPReq

-- We need to pass in the constrained type variable a here, because our newly boxed up existential type doesn't allow us to
-- pattern match on its internal type variable...
class RequestType a => Handleable a where
  handleReq ::  a -> HTTPReq -> IO ()

-- Definitions for the actual RequestType variants
instance Handleable GetRequest where
  handleReq GetRequest req = putStrLn $ "GET request to path " ++ (path req)

instance Handleable PostRequest where
  handleReq PostRequest req = putStrLn $ "POST request to path " ++ (path req)

-- DEMO: ADD INSTANCE AND SHOW CALL

-- A wrapper for the handleReq instance allows us to avoid needing to know how this mechanism works
-- at the call site.
handle :: HTTPReq -> IO ()
handle r@(HTTPReq a _) = handleReq a r

-- We have to add a new typeclass and existential wrapper type for each operation. That an operation requires a
-- typeclass is, again, because we're decoupling the operation from the fake sum type so more can be added ad hoc.
-- That we need an existential wrapper per operation is due to the fact that we need to revisit our type variable
-- constraints as we add functionality.

data ErrorReq = forall a. Errorable a => ErrorReq {
   methodE :: a
  ,pathE :: String
}

class RequestType a => Errorable a where
  handleError :: a -> ErrorReq -> IO ()
  
  -- Note: conversion not really possible here, as there's no information about a being Handleable
  -- convert :: a -> ErrorReq -> HTTPReq
  -- convert _ (ErrorReq a p) = HTTPReq a p

instance RequestType ErrorReq

instance Errorable GetRequest where
  handleError GetRequest req = putStrLn $ "Crap! Can't GET " ++ (pathE req)

instance Errorable PostRequest where
  handleError PostRequest req = putStrLn $ "Crap! Can't POST " ++ (pathE req)

handleE :: ErrorReq -> IO ()
handleE r@(ErrorReq a _) = handleError a r

defaultMethods :: M.Map String HTTPReq
defaultMethods = M.fromList [
                               ("GET", HTTPReq GetRequest "/cool")
                              ,("POST", HTTPReq PostRequest "/hi/john")
                            ]

getByMethod :: String -> Maybe HTTPReq
getByMethod method = M.lookup method defaultMethods

-- the following will produce an error because we're trying to pattern match on the
-- components of the existential, which are unknown to the compiler due to type erasure.
--
-- doStuff :: HTTPReq -> IO ()
-- doStuff (HTTPReq GetRequest _) = putStrLn "GET REQ"
-- doStuff (HTTPReq PostRequest _) = putStrLn "POST REQ"
--
-- here, we'll get an error because we're trying to pattern match on
-- completely separate types.
-- doStuff :: HTTPReq -> IO ()
-- doStuff (HTTPReq a p) = doStuff' a
--   where doStuff' GetRequest = putStrLn $ "OK, Get to "++p
--         doStuff' PostRequest = putStrLn $ "OK, Post to "++p
--
-- Likewise, the following won't work. Again, we have a problem arising from our
-- implicit use of universal quantification.
-- doStuff :: HTTPReq -> IO ()
-- doStuff req@(HTTPReq a _) = doStuff' a req
--
-- doStuff' :: RequestType a => a -> HTTPReq -> IO ()
-- doStuff' GetRequest r = putStrLn $ "GET " ++ (path r)
-- doStuff' PostRequest r = putStrLn $ "POST " ++ (path r)
