    checkIfHot :: String -> String
    checkIfHot isHot =
        if isHot == "Hot"
            then "Break out the BBQ!"
            else "Nope go back inside"


    specialBirthday :: Int -> [Char]
    specialBirthday age
        | age == 1 = "First birthday!"
        | age == 18 = "You're an adult!"
        | age == 60 = "Finally, I can stop caring about new lingo!"
        | otherwise = "Nothing special"

    returnHotterTempInK :: Double -> Double -> Double
    returnHotterTempInK freedomUnits celcius = if (freedomUnits - 32) * 5/9 >= celcius
                                                    then ((freedomUnits - 32) * 5/9) + 273.16
                                                    else celcius + 273.16

    returnHotterTempInK' :: forall a. (Ord a, Floating a) => a -> a -> a
    -- returnHotterTempInK' freedomUnits celcius = if (freedomUnits - 32) * 5/9 >= celcius
    --                                                 then ((freedomUnits - 32) * 5/9) + 273.16
    --                                                 else celcius + 273.16    
    returnHotterTempInK' freedomUnits celcius =
        let fToC t = (t - 32) * 5/9
            cToK t = t + 273.16
            fToK t = cToK (fToC t)
        in if celcius > fToC freedomUnits then cToK celcius else fToK freedomUnits
    
    returnHotterTempInK'' :: forall a. (Ord a, Floating a) => a -> a -> a
    returnHotterTempInK'' freedomUnits celcius = if celcius > fToC freedomUnits then cToK celcius else fToK freedomUnits
        where
            fToC t = (t - 32) * 5/9
            cToK t = t + 273.16
            fToK t = cToK (fToC t)

 