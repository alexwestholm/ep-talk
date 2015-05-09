-- our request types are no longer variants of a sum type
-- but distinct types of their own
data GetRequest = GetRequest deriving (Eq, Show)
data PostRequest = PostRequest deriving (Eq, Show)

-- we use an empty typeclass to fake a sum type, as
-- typeclasses are both open for extension and a
-- union of their instance types.
class RequestType a
instance RequestType GetRequest
instance RequestType PostRequest

-- we can embed them as type variables within other types
data HTTPReq a = HTTPReq {
  method :: a
 ,path :: String
} deriving (Eq, Show)

-- each operation becomes a typeclass with the constraint that
-- its parameter is an instance of our fake sum type.
class RequestType a => Handleable a where
  handleReq :: HTTPReq a -> IO ()

-- which allows us to easily extend the operation to account
-- for new instances as they arise.
instance Handleable GetRequest where
  handleReq r = putStrLn $ "Got a GET to " ++ (path r)

instance Handleable PostRequest where
  handleReq r = putStrLn $ "Got a POST to " ++ (path r)

-- likewise, we can add operations at will, and simply
-- need to cover all cases with respect to the typeclass
-- constraint of our fake sum type.
class RequestType a => Errorable a where
  ohno :: HTTPReq a -> IO ()

instance Errorable GetRequest where
  ohno r = putStrLn $ "Crap! Can't GET to "++(path r)

instance Errorable PostRequest where
  ohno r = putStrLn $ "Crap! Can't POST to "++(path r)
