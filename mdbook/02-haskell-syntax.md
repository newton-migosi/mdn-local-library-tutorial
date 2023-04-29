## Squeezing into Nine Constructors

### Switching it up

Contemporary classical computers operate on Binary Data.
Typically, these are said to be "on" or "off" states and are represented as 1 and 0 respectively.

Programming languages have a similar notion, usually called Booleans.
Let's define boolean values in Haskell that represents a computer bit in the "on" state

```hs
on = True
```

Every variable in Haskell has an associated type,
most of the time the compiler can deduce the type of the variable, so we can skip writing it the types.
If we add a type annotation to the variable, the compiler will check that our value matches the type of the variable and throw an error if they don't match.

So we can write

```hs
on :: Bool
on = True
```

The first line simply says that `x` is of type `Bool`. Here `Bool` is the name that was given to Boolean types in Haskell.

So now that we have booleans, let's use another common language feature : if-expressions

```hs
y :: Bool
y = if True then True else False
```

We can replace the first `True` with any expression that evaluates to a boolean. The other requirement is that the expressions we use in the `then` and `else` clauses should evaluate to the same type.

Let's add more complexity by using another common language feature : functions

```hs
y :: Bool -> Bool
y x = if x then True else False
```

This time `y`'s type annotation includes an arrow between two `Bool`s.
This type says that `y` is a function that when given a Boolean value will return a Boolean value.
We can use `y` somewhere else in the program and pass it a Boolean value.

```
z :: Bool
z = y False
```

Notice that function calls in Haskell do not use brackets around function arguments unlike other common languages where you'd expect `z = y(False)`. (What's the value of `z`?)

We've used the tokens `Bool`, `True` and `False` so far, without asking where these are from. Booleans are not part of the language, they're defined as part of the standard library simply as

```hs
data Bool = True | False
```

The data keyword introduces custom types into scope, here the custom type is `Bool`
The part to the right of the equation then gives the shape of the type
`True` and `False` are called the type constructors of `Bool`,
Type constructors are separated by the pipe symbol in a type definition to denote that a value of the given type can be created using one of the tokens defined

Now that we know the definition, we can use another important haskell language feature : case-expressions. In other languages this may be analogous to `match` statements in C-style languages. In fact we've already implicitly used a case-expresssion when we used the `if .. then .. else ..` clause. Haskell doesn't implement if statements natively rather they're converted into case statements.

For example, we can convert our previous example to use case by writing

```hs
y :: Bool -> Bool
y x = case x of
    True -> True
    False -> False
```

On the left hand side of the arrows are the constructors of the datatype we're matching against. On the right hand side of the arrows are the result expressions we want to return. All branches of case expressions must have the same type.

For simple types like Bool we can use another shorthand that's common in Haskell for doing case analysis. We can perform case analysis right in the function definition. We can rewrite this as

```hs
y :: Bool -> Bool
y True = True
y False = False
```

At this point it's quite obvious that `y` is in fact the identity function on Booleans. From now on we'll refer to `y` and any other identity function as `id` (Why would we want to define a function that does nothing?)

We can define another common boolean operator (not) as such

```hs
not :: Bool -> Bool
not True = False
not False = True
```

We have thus defined all possible functions with the type `Bool -> Bool`. In fact, some would argue that we have defined too many functions of type `Bool -> Bool`, because we could just have defined `id` as

```hs
id :: Bool -> Bool
id x = not (not x)
```

Booleans also have well known binary operators, let's define `and`

```hs
and :: Bool -> Bool -> Bool
and True True = True
and True False = False
and False False = False
and False True = False
```

Here we see our first multi-parameter function (arity > 1),
the type signature says that `and` takes one `Bool` then another `Bool` then returns one `Bool` as its result

It's well known that all binary operators on Booleans can be defined using `not` and `and`.
In fact, at some lower level the CPU you're using only knows how to perform a combination of these two functions, called `nand`

```hs
nand :: Bool -> Bool -> Bool
nand x y = not (and x y)
```

We can use nand to implement `or`

```hs
or :: Bool -> Bool -> Bool
or x y = nand (nand x x) (nand y y)
```

Exercise (Can you give a direct implementation of `or` that uses case expressions? Verify that our version that uses `nand` matches the direct implementation.)

Exercise (Implement `xor` `and` `nor` `xnor` `not` using the direct definition of `nand`)

### Grab a Byte

Something we can do with binary digit that doesn't exactly map to boolean values is addition.
In the binary system we have `0 + 0 = 0; 0 + 1 = 1; 1 + 0 = 1; 1 + 1 = 10;`
the last equation causes us problems because it actually needs two bits to represent the whole answer.

To make addition standard over all bits we'll extend the result of all the other bits to also be represented as two bits
`0 + 0 = 00; 0 + 1 = 01; 1 + 0 = 01 ; 1 + 1 = 10;`
At this point we can notice that the first bit is equivalent to the `and` of the inputs , while the second bit is the `xor` of the inputs;

In Haskell we can produce two outputs by placing them in a tuple, for example

```hs
addWithCarryBit :: Bool -> Bool -> (Bool, Bool)
addWithCarryBit x y = (and x y, xor x y)
```

The next logic gate we'll implement is the half-adder, this

What's better than one boolean value? Eight!

### The List of my concerns

We have played around with boolean values so far but it gets even more interesing when we have more multiple values together.
Enter the `List` datatype.

We can define a list of all possible Booleans as so

```hs
allBools :: [Bool]
allBools = [True, False]
```

The syntax `allBools :: [Bool]` means that `allBools` has type List of Bools, square brackets are used in haskell to represent lists.
In Haskell lists literals can be created using comma-separated values wrapped in square brackets.

If we don't have any bools at the moment we could say

```hs
gotNoBools :: [Bool]
gotNoBools = []
```

Computers represent numbers using Binary numbers, we can represent digits in the binary number system by having `0` being `False` and `1` being `True`.

So we could have define the first non-negative numbers up to five as

```hs
zero :: [Bool]
zero [False] -- 0

one :: [Bool]
one = [True] -- 1

two :: [Bool]
two = [True, False] -- 10

three :: [Bool]
three = [True, True] -- 11

four :: [Bool]
four = [True, False, False] -- 100

five :: [Bool]
five = [True, False, True] -- 101
```

We can perform case analysis on List of Bools similar to how we did on Just `Bool`s, for example we can define the predicate on List of Bools as

```hs
isEmpty :: [Bool] -> Bool
isEmpty [] = True
isEmpty _ = False
```

The first line gives the type of `isEmpty`, this function takes a List of Bools and returns a Bool
The second line says that all empty lists, such as `gotNoBools` satisfy the predicate.. which we indicate by returning `True`
The third line says that any other list is not empty, this last clause uses an underscore to match against every possible pattern.

We can see an example of this in

```hs
alwaysTrue :: [Bool] -> Bool
alwaysTrue _ = True
```

`alwaysTrue` returns True, no matter what the input is.

when given a list we can select elements from the list at various positions by pattern matching against the list

```hs
firstDigit :: [Bool] -> Bool
firstDigit (x:_) = x

secondDigit :: [Bool] -> Bool
secondDigit (_:x:_) = x

thirdDigit :: [Bool] -> Bool
thirdDigit (_:_:x:_) = x
```

Notice how the patterns for each position corresponds to how many underscores come before the digit we are looking for.
So the first digit doesn't have any underscores before it, while the second has one, and the third has two underscores before it.
When pattern matching a list we separate the patterns in the list using colons (:) as in the expression `(_:x:_)`.

If we're going to be using `[Bool]` to represent numbers then it'd be more convenient to give it a name that tells us that we're considering them as numbers
We can do so using type synonyms

```hs
type Number = [Bool]
```

Because of how lists work, we'll have to change our representation of numbers to start with the least significant bit first (change endianness)
so now we have `one = [True]; two = [True, False]; three = [True, True]`

We can then implement a function that computes the two's complement of the binary numbers in our system

```hs
twosComplement :: Number -> Number
twosComplement [] = []
twosComplement (x:rest) = not x : twosComplement rest
```

````hs
add :: Number -> Number -> Number
add [] [] = []
add (x:restX) (y:restY) =
    case carryBit x y of
        True ->
        False -> bitAdd x y : add restX restY








### I don't know, Maybe?



### Run the numbers

Haskell has all the usual data types and control flow you'd expect in a programming language.

We can define numbers, strings and booleans without any fuss
```hs
x = 5
y = "Hello"
z = True
````

We can optionally add type annotations to our variable definitions,
the compiler checks that the annotated type is equivalent to the actual type of the variable.

```hs
x :: Int
x  = 5

y :: String
y = "Hello"

z :: Bool
z = True

w :: Int
w = "5" -- Compiler will throw an error
```

Variables in Haskell are immutable by default, so using the same variable name twice in the same scope will be rejected by the compiler.

```hs
x = 5
x = 6 -- Compiler will throw an error
```

### No Strings attached

### Disfunctional

### Bonus 1 : Get in Line (Optimizations)

In the section covering booleans, we had the example

```hs
y :: Bool -> Bool
y x = if x then True else False

z :: Bool
z = y False
```

### Bonus 1.5 : Vecorize lists

### Bonus 2 : That's my Type

Our first section covered Boolean values defined as

```hs
data Bool = True | False
```

It stands to reason that there are only two possible values that can have type `Bool`.

Let's imagine that there existed a way for us to ask how many values are in a type.
Let's call that function `sizeOf`. Then we would say `sizeOf Bool == 2`

There are infinitely many integers so we'd expect `sizeOf Int == inf` where `inf` is infinity.

We may then ask which types `T` have `sizeOf T == 1` or `sizeOf T == 0`. In Haskell these types are known as `Unit` and `Void`. They can be defined as

```hs
data Unit = Unit

data Void
```

Notice how `Void` doesn't have a right-hand side definition, this is not a mistake. `Void` is a type that by it's nature cannot be constructed. It has no values in it! (Why would such a type exist?)

Let's make it more interesting by asking how many values can a Maybe type have?
Well, Maybe's are parametrized by a type

### Bonus 3 : The Power Bottom

The
