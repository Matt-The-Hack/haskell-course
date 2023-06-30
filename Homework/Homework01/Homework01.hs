
-- Question 1
-- Write a multiline comment below.

{- testing 
1 
2
3
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.

multiplyBy3 x = x * 3

-- Question 3
-- Define a function that calculates the area of a circle.

areaOfACircle r = pi * r^2

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 

areaofACylinder r h = areaOfACircle r * h

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.

meaningOfLife r h = areaofACylinder r h >= 42