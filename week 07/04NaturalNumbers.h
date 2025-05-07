data Nat = Zero | Succ Nat

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n


sumTwoNatNumbers :: Nat -> Nat -> Int
sumTwoNatNumbers Zero y = natToInt y
sumTwoNatNumbers x Zero = natToInt x
sumTwoNatNumbers x y = (natToInt x) + (natToInt y)

multiplyTwoNatNumbers :: Nat -> Nat -> Int
multiplyTwoNatNumbers Zero _ = 0
multiplyTwoNatNumbers _ Zero = 0
multiplyTwoNatNumbers x y = (natToInt x) * (natToInt y)

main :: IO ()
main = do
    print(multiplyTwoNatNumbers (Succ Zero) (Succ (Succ Zero)))
