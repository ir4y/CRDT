module Lens where

type Lens a b = ( a -> b , b -> a -> a )
getL :: Lens a b -> a -> b
getL (g, _) = g

setL :: Lens a b -> b -> a -> a
setL (_, s) = s

-- Apply a function to the field
-- I consider modL more fundamental than setL
modL :: Lens a b -> (b -> b) -> a -> a
modL l f a = setL l (f (getL l a)) a


-- Lens combination operator
(<.>):: Lens a b -> Lens b c -> Lens a c
(getA, setA) <.> (getB, setB) = (getB . getA, \z x -> setA (setB z (getA x)) x)

-- get value from object of type 'a' via lens 'l'
(^.) :: a -> Lens a b -> b
a ^. l  = getL l a

-- set value in object if type 'a' via lens 'l'
(^=) :: Lens a b -> b -> a -> a
(l ^= b) a = setL l b a

-- map value from object of type 'a' with function 'f' via lens 'l'
(%=) :: Lens a b -> (b -> b) -> a -> a
(l %= f) a = modL l f a
