# Primitives
This section will show each constructor for the abstract syntax tree and it's
corresponding JSON. In general the language for propositions is a boolean
language that sits on top of a arithmetic language, where the normal form is a
boolean literal or reference and the only way to use the arithmetic language is
via a binary operator that results in a boolean. The type for the boolean
language is `VProp a b` where `a` and `b` are type variables, likewise the type
for the arithmetic language is `VIExpr a`, where `a` is also a type variable.

### Literals
Literals are either Integers, Doubles, or booleans. For each example I'll show
either the AST or smart constructors to make the terms in haskell, and then the
invocation to view the JSON translation, see the `REPL` section in the docs for
instructions on how to do this yourself.

```
# Haskell
LitI (D Double)
LitI (I Integer)
LitB True

# JSON
> print $ encodePretty (LitI (D 2.718) :: VIExpr Var)
{
    "tag": "LitI",
    "contents": {
        "tag": "D",
        "contents": 2.718
    }
}

> print $ encodePretty (LitI (I 218) :: VIExpr Var)
{
    "tag": "LitI",
    "contents": {
        "tag": "I",
        "contents": 218
    }
}

> print $ encodePretty (LitB True :: VProp Var Var)
{
    "tag": "LitB",
    "contents": true
}
```

### Variables
```
# haskell
bRef "x" # defining a boolean variable named "x"
iRef "y" # defining a integer variable named "y"
dRef "z" # defining a double variable named "z"



> print $ encodePretty (bRef "x" :: VProp String String)
{
    "tag": "RefB",
    "contents": "x"
}


> print $ encodePretty (iRef "y" :: VIExpr String)
{
    "tag": "Ref",
    "contents": [
        "RefI",
        "y"
    ]
}

> print $ encodePretty (dRef "z" :: VIExpr String)
{
    "tag": "Ref",
    "contents": [
        "RefD",
        "z"
    ]
}
```

### Boolean Operators
The Boolean language provides a wide arrange of operators, due to the design of
the AST I've bucketted these by their arity. So you'll see all the generated
JSON will reflect this fact e.g. `OpBB` is a binary operator, that takes two
booleans and returns a boolean, while `OpIB` takes two arithmetic values and
returns a boolean. In addition, these constructors take the actual function to
execute. These will be things like `Not` or `Impl` for negation and implication.

```
# Haskell: Boolean Not
bnot (LitB False)

# JSON
> print $ encodePretty $ (bnot (LitB False) :: VProp Var Var)
{
    "tag": "OpB",
    "contents": [
        [],
        {
            "tag": "LitB",
            "contents": false
        }
    ]
}

# Haskell: Logical And
true &&& ChcB "DD" (bRef "x") (bRef "y")

# JSON
> print $ encodePretty $ (true &&& ChcB "DD" (bRef "x") (bRef "y") :: VProp String String)
{
    "tag": "Opn",
    "contents": [
        "And",
        [
            {
                "tag": "LitB",
                "contents": true
            },
            {
                "tag": "ChcB",
                "contents": [
                    {
                        "dimName": "DD"
                    },
                    {
                        "tag": "RefB",
                        "contents": "x"
                    },
                    {
                        "tag": "RefB",
                        "contents": "y"
                    }
                ]
            }
        ]
    ]
}

# Haskell: Logical Or
(ChcB "DD" true false ||| ChcB "DD" (bRef "x") (bRef "y") :: VProp String String)

# JSON
> print $ encodePretty $ (ChcB "DD" true false ||| ChcB "DD" (bRef "x") (bRef "y") :: VProp String String)
{
    "tag": "Opn",
    "contents": [
        "Or",
        [
            {
                "tag": "ChcB",
                "contents": [
                    {
                        "dimName": "DD"
                    },
                    {
                        "tag": "LitB",
                        "contents": true
                    },
                    {
                        "tag": "LitB",
                        "contents": false
                    }
                ]
            },
            {
                "tag": "ChcB",
                "contents": [
                    {
                        "dimName": "DD"
                    },
                    {
                        "tag": "RefB",
                        "contents": "x"
                    },
                    {
                        "tag": "RefB",
                        "contents": "y"
                    }
                ]
            }
        ]
    ]
}

# Haskell: Logical Implication
true ==> false

# JSON
> print $ encodePretty $ (true ==> false :: VProp Var Var)
{
    "tag": "OpBB",
    "contents": [
        "Impl",
        {
            "tag": "LitB",
            "contents": true
        },
        {
            "tag": "LitB",
            "contents": false
        }
    ]
}

# Haskell: Logical Equivalence
true <=> false

# JSON
> print $ encodePretty $ (true <=> false :: VProp Var Var)
{
    "tag": "OpBB",
    "contents": [
        "BiImpl",
        {
            "tag": "LitB",
            "contents": true
        },
        {
            "tag": "LitB",
            "contents": false
        }
    ]
}

# Haskell: Logical Xor
ChcB "Variation!" true (bRef "a") <+> bRef "y"

# JSON
> print $ encodePretty $ (ChcB "Variation!" true (bRef "a") <+> bRef "y":: VProp String String)
{
    "tag": "OpBB",
    "contents": [
        "XOr",
        {
            "tag": "ChcB",
            "contents": [
                {
                    "dimName": "Variation!"
                },
                {
                    "tag": "LitB",
                    "contents": true
                },
                {
                    "tag": "RefB",
                    "contents": "a"
                }
            ]
        },
        {
            "tag": "RefB",
            "contents": "y"
        }
    ]
}
```

### Arithmetic to Boolean Operators
These are operators that take two numeric values and produce a boolean.

```
# Haskell: Less than, This applies to greater than, greater than equal
# , equal, and not equal as well
dRef "x" .< iRef "y"

# JSON
> print $ encodePretty $ (dRef "x" .< iRef "y":: VProp String String)
{
    "tag": "OpIB",
    "contents": [
        "LT",
        {
            "tag": "Ref",
            "contents": [
                "RefD",
                "x"
            ]
        },
        {
            "tag": "Ref",
            "contents": [
                "RefI",
                "y"
            ]
        }
    ]
}

# Haskell: greater than, less than equal, and not equal
dRef "x" .> iRef "y"
dRef "x" .<= iRef "y"
dRef "x" ./= iRef "y"

# JSON
> print $ encodePretty $ (dRef "x" .> iRef "y":: VProp String String)
{
    "tag": "OpIB",
    "contents": [
        "GT",
        {
            "tag": "Ref",
            "contents": [
                "RefD",
                "x"
            ]
        },
        {
            "tag": "Ref",
            "contents": [
                "RefI",
                "y"
            ]
        }
    ]
}

> print $ encodePretty $ (dRef "x" .<= iRef "y":: VProp String String)
{
    "tag": "OpIB",
    "contents": [
        "LTE",
        {
            "tag": "Ref",
            "contents": [
                "RefD",
                "x"
            ]
        },
        {
            "tag": "Ref",
            "contents": [
                "RefI",
                "y"
            ]
        }
    ]
}

> print $ encodePretty $ (dRef "x" ./= iRef "y":: VProp String String)
{
    "tag": "OpIB",
    "contents": [
        "NEQ",
        {
            "tag": "Ref",
            "contents": [
                "RefD",
                "x"
            ]
        },
        {
            "tag": "Ref",
            "contents": [
                "RefI",
                "y"
            ]
        }
    ]
}
```

### Arithmetic Unary Operators

```
# Haskell: Negation, Absolute value and Signum
negate (dRef "x")
abs (iRef "y")
signum (LitI (D 2.718))

# JSON
> print $ encodePretty $ (negate (dRef "x"))
{
    "tag": "OpI",
    "contents": [
        "Neg",
        {
            "tag": "Ref",
            "contents": [
                "RefD",
                "x"
            ]
        }
    ]
}

> print $ encodePretty $ (abs (iRef "y"))
{
    "tag": "OpI",
    "contents": [
        "Abs",
        {
            "tag": "Ref",
            "contents": [
                "RefI",
                "y"
            ]
        }
    ]
}

> print $ encodePretty $ (signum (iRef "y"))
{
    "tag": "OpI",
    "contents": [
        "Sign",
        {
            "tag": "Ref",
            "contents": [
                "RefI",
                "y"
            ]
        }
    ]
}
```
