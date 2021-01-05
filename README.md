# Numeric Typeclasses

This pacakge attempts to model the numeric hierachay found in abstract algebra.  

These mathemtaical ojbects are defined as sets with accompanying closed binary operations that also, depending on the object, obey certain propetries such
as assosiativity, commutivity, and distributative laws.

For example the most elementary mathematical object is called a Magma and it is defined as a set with a closed binary operation.  However the binary operation associated with the Magma does **Not** have to conform to the assosiative property.  The Magma is the most elementary mathemtaical objects and is used here for only illustrative purposes.  It does not have much value in math or programming and so this library starts its hierarchy with the Semigroup.

## Semigroup

    * A Set  
    + Closed binary operation  
    + Binary operation conforms to the associative property  

## CommutativeSemigroup

    + Semigroup
    + Semigroup binary operation conforms to the commutative property

## Monoid

    + Semigroup
    + Identity value

## CommutativeMonoid

    + Monoid
    + Monoid binary operation conforms to the commutative property

## Group

    + Monoid
    + Inverses

## AbelianGroup

    + Group
    + Group binary operation conforms to the commutative property

## Semring

    + Two Binary operations
        + Addition
            + CommutativeMonoid
        + Multiplication
            + Monoid

## Ring

    + Two Binary operations
        + Addition
            + AbelianGroup
        + Multiplication
            +Monoid

## CommutativeRing

    + Ring
    + Ring multiplication operation conforms to the commutative property

## DivisionRing

    + Ring
    + Ring multiplication is a Group

## CommutativeDivisionRing

    + DivisionRing
    + Ring multiplication operation conforms to the commutative property

## Field

    + CommutativeDivisionRing
