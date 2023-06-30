-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
    consumptionOfElectricty :: forall a. (Ord a, Show a, Num a) => a -> a -> a -> String
    consumptionOfElectricty kW h allowedAmount 
            | totalUsed > allowedAmount = "TURN IT OFF " ++ show totalUsed
            | totalUsed == allowedAmount = "Just right! " ++  show totalUsed
            | otherwise = "Saving that Money! " ++ show totalUsed
        where totalUsed = (kW * h) * 30
    
-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.


-- In the previous function, return the excess/savings of consumption as part of the message.

--look above

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.

    calculateAreaOfHouse :: (Fractional a, Show a) => a -> a -> a -> a -> String
    calculateAreaOfHouse heightRectangle widthRectangle baseTriangle heightTriangle =
        let rectangle = heightRectangle * widthRectangle
            triangle = 1/2 * baseTriangle * heightTriangle
        in show (rectangle + triangle)

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.\

    quotientLessThanOne :: (Fractional a, Ord a, Show a) => a -> a -> String
    quotientLessThanOne numOne numTwo
        | numOne == 0 || numTwo == 0 = "Cannot divide by 0"
        | numOne > numTwo = show (numTwo/numOne)
        | numTwo > numOne = show (numOne/numTwo)
        | otherwise = show 1

    quotientLessThanOne' :: forall a. (Fractional a, Ord a, Show a) => a -> a -> String
    quotientLessThanOne' numOne numTwo =
      if numOne == 0 
        then "Cannot divide by 0"
        else
          if numOne > numTwo
            then show (numTwo/numOne)
            else
              if numTwo > NumOne 
                then show (numOne/numTwo)
                else show 1

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.

    calculateSumOfSquares :: Fractional a => a -> a -> a
    calculateSumOfSquares a b = let product = a * b
                                    quotient = a / b
                                in  calculateSum product quotient
                                where
                                  calculateSum :: a -> a -> a
                                  calculateSum x y =
                                    let squareOne = x ** 2
                                        squareTwo = y ** 2
                                    in squareOne + squareTwo


  