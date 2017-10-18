-- Some examples for the use of CurryCheck with user-defined data

import Test.Prop

-- Natural numbers defined by s-terms (Z=zero, S=successor):
data Nat = Z | S Nat
 deriving (Eq,Show)

-- addition on natural numbers:
add         :: Nat -> Nat -> Nat
add Z     n = n
add (S m) n = S(add m n)

-- subtraction is defined by reversing the addition:
sub x y | add y z == x  = z where z free

-- less-or-equal predicate on natural numbers:
leq Z     _     = True
leq (S _) Z     = False
leq (S x) (S y) = leq x y


-- Property: the addition operator is commutative
add_commutative x y = add x y -=- add y x

-- Property: the addition operator is associative
add_associative x y z = add (add x y) z -=- add x (add y z)

-- Properties: subtracting a value which was added yields the same value
sub_addl x y = sub (add x y) x -=- y

sub_addr x y = sub (add x y) y -=- x

-- Property: adding a number yields always a greater-or-equal number
leq_add x y = always $ leq x (add x y)
