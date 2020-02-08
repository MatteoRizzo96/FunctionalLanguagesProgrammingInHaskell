module Validate where 

{-
    Validating Credit Card Numbers

    A validation algorithm for credit cards. It follows these steps:
    1. Double the value of every second digit beginning from the right. That is, the last digit is unchanged;
       the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example, 
       [1,3,8,6] becomes [2,3,16,6].
    2. Add the digits of the doubled values and the undoubled digits from the original number. 
       For example, [2,3,16,6] becomes 2+3+1+6+6 = 18.
    3. Calculate the remainder when the sum is divided by 10. For the above example, the remainder would be 8.
    4. If the result equals 0, then the number is valid.
-}

-- EXERCISE 1

{-
    Convert positive Integers to a list of digits or to the reversed list, respectively.
    For 0 or negative inputs, return the empty list.
    
    Example: toDigits 1234 == [1,2,3,4]
    Example: toDigits 0 == []
    Example: toDigits (-17) == []
    Example: toDigitsRev 1234 == [4,3,2,1]
-}

toDigits :: Integer -> [Integer]
toDigits x = if x <= 0 then [] else [read [c] | c <- show x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

-- EXERCISE 2

{-
    Doubles every other number beginning from the right, that is,
    the second-to-last, fourth-to-last, ... numbers are doubled.
    
    Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
    Example: doubleEveryOther [1,2,3] == [1,4,3]
-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [if mod i 2 == 0 then x else 2 * x  | (x, i) <- zip (reverse xs) [0..]]

-- EXERCISE 3

{-
    Calculate the sum of all digits in a list of integers.

    Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
 -}

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sum (toDigits x) | x <- xs]

-- EXERCISE 4

{-
    Indicates whether an Integer could be a valid credit card number.

    Example: validate 4012888888881881 = True
    Example: validate 4012888888881882 = False
-}

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0