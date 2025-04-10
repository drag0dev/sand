# Sand
Sand is a command-line calculator that handles numbers of arbitrary width and allows mixing different number formats. The project grew out of a need to quickly test bit manipulation operations with arbitrary-precision numbers.

## Features
- Decimal, binary, octal and hex numbers
- Arbitrary-width numbers
- Reserved keyword ```ans``` to reuse the previous result
- Reserved keyword ```exit``` 

## Limitations and special behavior
- Only whole numbers
- Positive and negative sign can be put in front of a number to signify the sign, whether there is a space or not
- No division or mod by zero
- No negative exponents when using pow
- Exponent has to fit into int63
- Cannot shift by a negative number
- Number of shifts has to fit into int63
- To specify number format use 0x/0b/0o or no prefix for decimal
- If there is a valid expression before finding `exit`, the expression is evaluated and then the calculator exits
- Whitespace-insensitive, except within numbers (no spaces mid-number)


## Requirements
- Ocaml (version 4.04.0 or later)
- Opam
- Dune
- Either the GMP library or the MPIR library, including development files
- GCC or Clang

## Installation
```
git clone https://github.com/drag0dev/sand/
cd ./sand
dune install --prefix=/usr/local (any dir will suffice)
```

## Examples
```
(ans: none) > (5 ** 5) - 2 ** 5
Dec: 3093
Hex: C15
Binary: 110000010101
Octal: 6025

(ans: 3093) > ans / 2
Dec: 1546
Hex: 60A
Binary: 11000001010
Octal: 3012

(ans: 1546) > ans | 1500
Dec: 2014
Hex: 7DE
Binary: 11111011110
Octal: 3736

(ans: 2014) > -ans + -ans
Dec: -4028
Hex: -FBC
Binary: -111110111100
Octal: -7674

(ans: -4028) > (ans % 10) + - (1&1)
Dec: -9
Hex: -9
Binary: -1001
Octal: -11

(ans: -9) > exit
```

## Operators and precedence  
Precedence from highest to lowest:  

| Operator             | Precedence |
|----------------------|------------|
| Positive sign (+)    | 5          |
| Negative sign (-)    | 5          |
| BitwiseComplement (~)| 5          |
| OpeningParenthesis \(| 4          |
| ClosingParenthesis \)| 4          |
| Pow (**)             | 3          |
| Multiplication (*)   | 2          |
| FloorDivision (/)    | 2          |
| Modulo (%)           | 2          |
| Addition (+)         | 1          |
| Subtraction (-)      | 1          |
| BitwiseAND (&)       | 1          |
| BitwiseOR (\|)       | 1          |
| BitwiseXOR (^)       | 1          |
| BitwiseNAND (!&)     | 1          |
| BitwiseNOR (!\|)     | 1          |
| BitwiseXNOR  (!^)    | 1          |
| ShiftLeft (<<)       | 1          |
| ShiftRight (>>)      | 1          |