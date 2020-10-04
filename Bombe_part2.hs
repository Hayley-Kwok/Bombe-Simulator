{-  Name: Hayley Wing Yin Kwok
    COM 2108 Functional Programming Assignment 3 Part 2:  Remove the simplication on rotors (no fixed rotors)-}

module Bombe where
    import AssignmentHelp
    import Enigma
    import Data.Char
    import Data.List
    import Data.Maybe
    
    type SteckerPair = (Char,Char)
    type RotorsPair = (Rotor,Rotor,Rotor)

    -- This function takes in a Crib (original message, encrypted message) and returns valid steckerboard, offset and RotorsPair if a solution exists
    -- It will returns nothing if no solution is found
    breakEnigma :: Crib -> Maybe (Offsets, Steckerboard,RotorsPair)
    breakEnigma (plain,cipher) 
      = breakRotor (plain,cipher) menu [(firstMenuChar,'A')] (0,0,0) 0  
      where
        menu = longestMenu (plain,cipher)
        firstMenuChar = plain!!(menu!!0)
     -- (rotor1,rotor2,rotor3) testing:
    -- breakEnigma ("AFJEQTMC","FJEQTMCF")
    --  return Just ((0,0,0),[('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('B','F')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE",
    --   "BDFHJLCPRTXVZNYEIWGAKMUSQO"))
    -- breakEnigma ("ZGXWAUTS","XKGZWAUT")
    --  return Just ((0,0,0),[('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE",
    --   "BDFHJLCPRTXVZNYEIWGAKMUSQO"))
    -- breakEnigma ("COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE","QWAVMZPNGFQVGWGYCKCXXHMEXTCGWPFOCWCSYXAEFUNXQFIZJW")
    --  return Just ((0,0,0),[('Q','C'),('W','I'),('X','A'),('U','E'),('H','S'),('G','T'),('R','M')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE",
    --   "BDFHJLCPRTXVZNYEIWGAKMUSQO"))
    -- (rotor1,rotor2,rotor4) testing:
    -- "FHXTNTDSELJJZPGTSPHMJPIHVXBFQWTYIUNPEDKMXXILQAEWUNKBYOEDVQJBQ" is the result of enigmaEncodeMessage "ALONGENOUGHMESSAGETHATWILLGIVEMETHECORRECTRESULTFINGERCROSSED" 
    --  (SteckeredEnigma rotor1 rotor2 rotor4 reflectorB (0,0,1) [('F','T'),('D','U'),('V','A'),('K','W')])
    -- breakEnigma ("ALONGENOUGHMESSAGETHATWILLGIVEMETHECORRECTRESULTFINGERCROSSED","FHXTNTDSELJJZPGTSPHMJPIHVXBFQWTYIUNPEDKMXXILQAEWUNKBYOEDVQJBQ")
    --   return Just ((0,0,1),[('K','W'),('V','A'),('D','U'),('F','T')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","ESOVPZJAYQUIRHXLNFTGKDCMWB")) \
    -- (rotor1,rotor2,rotor5) testing:
    -- EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP" is the result of enigmaEncodeMessage  "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION" 
    --   (SteckeredEnigma rotor1 rotor2 rotor5 reflectorB (0,0,0) [('H','Z'),('I','X')])
    -- breakEnigma ("ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP")
    --   return Just ((0,0,0),[('X','I'),('Z','H')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","VZBRGITYUPSDNHLXAWMJQOFECK"))


    -- Assumption: At the first call, the count integer has to be 0 
    -- This function will try all the permutation of choosing three offsets from the five offsets until the function return a result or return nothing when it fails 
    --  after trying the last set of rotors (rotor5,rotor4,rotor3)       
    breakRotor :: Crib -> Menu -> Steckerboard -> Offsets -> Int -> Maybe (Offsets, Steckerboard,RotorsPair)
    breakRotor crib menu stecker offsets count
        | (count >= 60) = Nothing
        | (nothingP currentEA) =  breakRotor crib menu stecker offsets (count+1)  
        | not(nothingP currentEA) = Just (successOffset,successStecker, currentRotors)
        | otherwise = Nothing
        where
            currentRotors = AssignmentHelp.fromMaybe (getRotors count)
            currentEA = breakEA crib menu stecker offsets currentRotors
            (successOffset,successStecker) = AssignmentHelp.fromMaybe currentEA
    --(rotor1,rotor2,rotor3) testing:    
    --  breakRotor ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) 0 
    --   return Just ((0,0,0),[('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE",
    --    "BDFHJLCPRTXVZNYEIWGAKMUSQO"))
    --  breakRotor ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,0) 0
    --   return Just ((0,0,0),[('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('B','F')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE",
    --    "BDFHJLCPRTXVZNYEIWGAKMUSQO")) 
    -- (rotor1,rotor2,rotor4) testing:
    -- "FHXTNTDSELJJZPGTSPHMJPIHVXBFQWTYIUNPEDKMXXILQAEWUNKBYOEDVQJBQ" is the result of enigmaEncodeMessage "ALONGENOUGHMESSAGETHATWILLGIVEMETHECORRECTRESULTFINGERCROSSED" 
    --  (SteckeredEnigma rotor1 rotor2 rotor4 reflectorB (0,0,1) [('F','T'),('D','U'),('V','A'),('K','W')]) 
    -- breakRotor ("ALONGENOUGHMESSAGETHATWILLGIVEMETHECORRECTRESULTFINGERCROSSED","FHXTNTDSELJJZPGTSPHMJPIHVXBFQWTYIUNPEDKMXXILQAEWUNKBYOEDVQJBQ") 
    --  [53,7,14,9,1,19,30,18,33,8,43,46,34,3,32,27,48,45,15,47,22,49,6,60] [('R','A')] (0,0,0) 0
    --   return Just ((0,0,1),[('K','W'),('V','A'),('D','U'),('F','T')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","ESOVPZJAYQUIRHXLNFTGKDCMWB"))
    -- (rotor1,rotor2,rotor5) testing:
    -- EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP" is the result of enigmaEncodeMessage  "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION" 
    --   (SteckeredEnigma rotor1 rotor2 rotor5 reflectorB (0,0,0) [('H','Z'),('I','X')])
    -- breakRotor ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") 
    --  [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] [('L','A')] (0,0,0) 0
    --   return Just ((0,0,0),[('X','I'),('Z','H')],("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","VZBRGITYUPSDNHLXAWMJQOFECK")) 

    -- This function checks all possible steckerboards for all possible offsets.
    -- It will returns nothing if no solution is found
    -- Assumption: At the first call of this function, the offset has to be (0,0,0)
    breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> RotorsPair -> Maybe (Offsets, Steckerboard)
    breakEA crib menu stecker origOffset rotors
        | (nextOffset == (0,0,0)) = Nothing
        | (nothingP successOffset) = breakEA crib menu stecker nextOffset rotors  
        | not(nothingP successOffset) = Just (origOffset,(AssignmentHelp.fromMaybe successOffset))
        | otherwise = Nothing
        where
            successOffset = findStecker crib menu stecker origOffset rotors
            nextOffset = advanceOffsets origOffset
    -- Success Case Testing: 
    -- breakEA ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) (rotor1,rotor2,rotor3)
    --  return Just ((0,0,0),[('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T')])
    -- breakEA ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,0) (rotor1,rotor2,rotor3)
    --   return Just ((0,0,0),[('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('B','F')])
    -- Failing Case Testing (no solution to the given Crib & Menu & rotors)
    --   (mean the system should have tried all combinations of the offset and initial pair of the steckerboard):
    -- breakEA ("ZGXWAUTS","AAAAAAAA") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) (rotor1,rotor2,rotor3) return Nothing
    --Other rotors Testing:
    --Success Case Testing:
    -- breakEA ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] 
    --   [('L','A')] (0,0,0) (rotor1,rotor2,rotor5)
    --  return Just [('X','I'),('Z','H')]
    --Failing Case Testing (wrong rotors):
    -- breakEA ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] 
    --  [('L','A')] (0,0,0) (rotor1,rotor2,rotor4)
    --  return Nothing

    -- This function checks all possible steckerboards for a given offset
    -- It will returns nothing if no solution is found
    -- Assumption: At the firts call, the steckerboard has to be [(initial,'A')] (it cannot be other character for the second element)
    findStecker :: Crib -> Menu -> Steckerboard -> Offsets -> RotorsPair -> Maybe Steckerboard
    findStecker crib menu [(initial,altering)] offset rotors
        | (alteringIndex+1) > 25 = Nothing 
        | (nothingP successStecker) = findStecker crib menu [(initial,nextTry)] offset rotors
        | not(nothingP successStecker) = Just (removeEqualStecker (AssignmentHelp.fromMaybe successStecker))
        | otherwise = Nothing
        where
            successStecker = followMenu crib menu [(initial,altering)] offset rotors
            alteringIndex = (elemIndices altering alphabet)!!0
            nextTry = alphabet!!(alteringIndex+1)
    --Success Case Testing:
    --  findStecker ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) (rotor1,rotor2,rotor3) 
    --   return Just Just [('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T')]
    --  findStecker ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,0) (rotor1,rotor2,rotor3)
    --   return Just [('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('B','F')]
    --Failing Case Testing (Wrong offset):
    --  findStecker ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,1) (rotor1,rotor2,rotor3)
    --   return Nothing
    --  findStecker ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (25,25,25) (rotor1,rotor2,rotor3)
    --   return Nothing
    --Failing Case Testing(no solution to the given Crib & menu):
    --  findStecker ("ZGXWAUTS","AAAAAAAA") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) (rotor1,rotor2,rotor3)
    --   return Nothing
    --Other rotors Testing:
    --Success Case Testing:
    -- findStecker ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] 
    --   [('L','A')] (0,0,0) (rotor1,rotor2,rotor5)
    --  return Just [('X','I'),('Z','H')]
    --Failing Case Testing (wrong offset):
    -- findStecker ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] 
    --  [('L','A')] (0,0,1) (rotor1,rotor2,rotor5)
    --  return Nothing
    --Failing Case Testing (wrong rotors):
    -- findStecker ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] 
    --  [('L','A')] (0,0,1) (rotor1,rotor2,rotor4)
    --  return Nothing

    -- With a given offset, menu and initial steckerboard, 
    -- this function recursively tries to add new pair to the existing steckerboard
    -- It will returns nothing if no solution is found
    followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> RotorsPair -> Maybe Steckerboard
    followMenu _ [] existingStecker _ _ = Just existingStecker
    followMenu (plain,cipher) (m:ms) existingStecker offset (firstRotor,secondRotor,thirdRotor)
        |(encodedChar == cipherChar) 
            = followMenu (plain,cipher) ms ((encodedChar,cipherChar):existingStecker) offset (firstRotor,secondRotor,thirdRotor)
        |not(nothingP newStecker) 
            = followMenu (plain,cipher) ms (AssignmentHelp.fromMaybe newStecker) offset (firstRotor,secondRotor,thirdRotor)
        | otherwise = Nothing      
        where  
            plainChar = plain!!m 
            steckerChar = if (checkStecker existingStecker plainChar) then (reflectorPair existingStecker plainChar) else plainChar
            correspondOffset = advanceMoreOffset offset m
            encodedChar = enigmaEncode steckerChar (SimpleEnigma firstRotor secondRotor thirdRotor reflectorB correspondOffset)
            cipherChar = cipher!!m
            newStecker = steckerAdd (cipherChar,encodedChar) existingStecker
    --Success Case Testing:    
    -- followMenu ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,0) (rotor1,rotor2,rotor3) 
    --  return Just [('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('J','J'),('B','F'),('A','A')]
    -- followMenu ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','S')] (0,0,0) (rotor1,rotor2,rotor3)
    --  return Just [('G','G'),('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T'),('S','S')]
    --Failing Case Testing (wrong Start Pair) :
    -- followMenu ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','A')] (0,0,0) (rotor1,rotor2,rotor3) 
    --  return Nothing
    -- followMenu ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','Z')] (0,0,0) (rotor1,rotor2,rotor3) 
    --  return Nothing
    --Failing Case Testing (wrong offset) : 
    -- followMenu ("AFJEQTMC","FJEQTMCF") [0,1,2,3,4,5,6,7] [('A','A')] (0,0,1) (rotor1,rotor2,rotor3)
    --  return Nothing
    -- followMenu ("ZGXWAUTS","XKGZWAUT") [7,6,5,4,3,0,2,1] [('S','S')] (25,25,8) (rotor1,rotor2,rotor3) 
    --  return Nothing
    --Other rotors Testing:
    --Success Case Testing:
    -- followMenu ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] 
    --   [('A','A')] (0,0,0) (rotor1,rotor2,rotor5)
    -- return Just [('C','C'),('W','W'),('B','B'),('O','O'),('E','E'),('N','N'),('A','A'),('U','U'),('T','T'),('S','S'),('T','T'),('X','I'),('E','E'),('O','O'),
    --   ('S','S'),('Z','H'),('A','A')]
    --Failing Case Testing (wrong offset):
    -- followMenu ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] 
    --  [('A','A')] (0,0,1) (rotor1,rotor2,rotor5)
    --  return Nothing
    --Failing Case Testing (wrong rotors):
    -- followMenu ( "ALONGMESSAGETHATWILLBEENOUGHFORTHESOLUTION","EYCENWIOTZDQSSNCCWBHWOFFPABWBBTPPFOEHSUTJP") [19,13,7,35,6,39,12,8,38,25,14,3,21,29,20,16] 
    --  [('A','A')] (0,0,1) (rotor1,rotor2,rotor4)
    --  return Nothing
            
    -- This function will add the given SteckPair to the Steckerboard if it is a valid input to the existing Steckerboard
    -- If the given SteckPair is invalid, the function will return nothing
    steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
    steckerAdd (x,y) existingStecker
        | not(checkStecker existingStecker x) && not(checkStecker existingStecker y) = Just ((y,x):existingStecker)
        | (equalStecker (x,y) existingStecker) = Just existingStecker
        | otherwise = Nothing
    -- Success Case Testing:
    --  steckerAdd ('O','U') [('A','H')] return Just [('U','O'),('A','H')]
    -- Border Case Testing (Same element to the existing pairs in the SteckerBoard ):
    --  steckerAdd ('A','H') [('A','H')] return Just [('A','H')]
    --  steckerAdd ('A','H') [('H','A')] return Just [('H','A')]
    -- Nothing Case Testing (Conflict pair to the existing pairs in the SteckerBoard):
    --  steckerAdd ('H','H') [('A','H')] return Nothing
    --  steckerAdd ('A','A') [('A','H')] return Nothing


    {----------------------------------------------------Helper Functions-------------------------------------}
    -- Return False if the given character does not exist in the Steckerboard 
    -- False example: checkStecker [('A','U'),('I','P')] 'O' return False
    -- True Example:  checkStecker [('A','U'),('I','P')] 'I' return True
    checkStecker :: Steckerboard -> Char -> Bool 
    checkStecker [] _ = False
    checkStecker ((plain,guess):restList) x 
        | (x == guess) || (x == plain) = True || checkStecker restList x
        | otherwise = False || checkStecker restList x
    
    -- Return True if the given SteckerPair exist in the Steckerboard 
    -- False example: equalStecker ('A','A') [('H','A'),('K','L')] return False
    -- True Example:  equalStecker ('A','H') [('H','A'),('K','L')] return True  
    equalStecker ::  SteckerPair -> Steckerboard -> Bool
    equalStecker _ [] = False
    equalStecker (x,y) ((plain,guess):restList)
        | ((x == plain) && (y == guess)) || ((x == guess) && (y == plain)) = True || equalStecker (x,y) restList
        | otherwise = False || equalStecker (x,y) restList
    
    -- Remove the SteckerPair from the Steckerboard if both elements in the SteckerPair are the same  
    -- Success Example: removeEqualStecker [('A','A'),('I','I'),('O','P')] return [('O','P')]
    -- Nothing to delete testing: removeEqualStecker [('O','P')] return [('O','P')]
    removeEqualStecker :: Steckerboard -> Steckerboard
    removeEqualStecker [] = []
    removeEqualStecker ((x,y):rest)
        | (x==y) = removeEqualStecker rest
        | otherwise = (x,y):(removeEqualStecker rest)

    -- Advance the offset by the amount of the given number
    -- Success Example: advanceMoreOffset (0,0,0) 9 return (0,0,9)
    -- Border Case testing: advanceMoreOffset (25,25,25) 9  return (0,0,8)
    advanceMoreOffset :: Offsets -> Int -> Offsets
    advanceMoreOffset offset 0 = offset
    advanceMoreOffset offset n = advanceMoreOffset (advanceOffsets offset) (n-1)
        
    allRotorList = [rotor1,rotor2,rotor3,rotor4,rotor5]

    -- Return the corresponding rotorPairs according to the number of count
    -- Will return nothing when count equal or larger than 60 
    -- True Case: getRotors 9
    --  return Just ("EKMFLGDQVZNTOWYHXUSPAIBRCJ","VZBRGITYUPSDNHLXAWMJQOFECK","AJDKSIRUXBLHWTMCQGZNPYFVOE")
    -- Nothing(Border Case): 
    --  getRotors 59 
    --   return Just ("VZBRGITYUPSDNHLXAWMJQOFECK","ESOVPZJAYQUIRHXLNFTGKDCMWB","BDFHJLCPRTXVZNYEIWGAKMUSQO")
    --  getRotors 60 return Nothing
    --  getRotors 61 return Nothing
    -- Failing Case:
    --  getRotors 1999 return Nothing
    getRotors :: Int -> Maybe RotorsPair
    getRotors count 
        | (count >= 60) = Nothing
        | (firstRotor /= Nothing) && (secondRotor /= Nothing) && (thirdRotor /= Nothing) 
            = Just (justFirstRotor,justSecondRotor, justThirdRotor) 
        | otherwise = Nothing
        where
            firstRotor = changeFirstRotor count
            justFirstRotor = AssignmentHelp.fromMaybe firstRotor
            secondRotor = changeSecondRotor count justFirstRotor
            justSecondRotor = AssignmentHelp.fromMaybe secondRotor
            thirdRotor = changeThirdRotor count justFirstRotor justSecondRotor
            justThirdRotor = AssignmentHelp.fromMaybe thirdRotor

    -- Take in the count and return the corresponding first rotor
    -- Will return nothing when count equal or larger than 60
    -- True Case: changeFirstRotor 0 return Just "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    -- Border Case: 
    --  changeFirstRotor 59 Just "VZBRGITYUPSDNHLXAWMJQOFECK" 
    --  changeFirstRotor 60 return Nothing
    --  changeFirstRotor 61 return Nothing
    -- Failing Case:
    --  changeFirstRotor 100 return Nothing         
    changeFirstRotor :: Int -> Maybe Rotor 
    changeFirstRotor count 
        | (count >= 0) && (count <12)  = Just rotor1
        | (count > 11) && (count <24)  = Just rotor2
        | (count > 23) && (count <36)  = Just rotor3
        | (count > 35) && (count <48)  = Just rotor4
        | (count > 47) && (count <60)  = Just rotor5
        | otherwise = Nothing
    
    -- Take in the count and the first Rotor then return the corresponding second rotor
    -- Will return nothing when count equal or larger than 60
    -- True Case: changeSecondRotor 0  rotor1 return Just "AJDKSIRUXBLHWTMCQGZNPYFVOE"
    -- Border Case:
    --  changeSecondRotor 59  rotor5 return Just "ESOVPZJAYQUIRHXLNFTGKDCMWB" 
    --  changeSecondRotor 60  rotor5 return Nothing   
    --  changeSecondRotor 61  rotor5 return Nothing    
    -- Failing Case: 
    --  changeSecondRotor 1000  rotor5
    changeSecondRotor :: Int -> Rotor ->Maybe Rotor 
    changeSecondRotor count firstRotor
        | (count >= 60) = Nothing
        | (secondRotorCount == 0) = Just (listWithoutFirstRotor!!0)
        | (secondRotorCount == 1) = Just (listWithoutFirstRotor!!1)
        | (secondRotorCount == 2) = Just (listWithoutFirstRotor!!2)
        | (secondRotorCount == 3) = Just (listWithoutFirstRotor!!3)
        | otherwise = Nothing
        where
            listWithoutFirstRotor = delete firstRotor allRotorList 
            round = count `mod` 12 
            secondRotorCount = round `div` 3
    
    -- Take in the count, the first Rotor and the second Rotor then return the corresponding third rotor
    -- Will return nothing when count equal or larger than 60
    -- True Case:  changeThirdRotor 11 rotor1 rotor5 return Just "ESOVPZJAYQUIRHXLNFTGKDCMWB" 
    -- Border Case:
    --  changeThirdRotor 59 rotor5 rotor4 return Just "BDFHJLCPRTXVZNYEIWGAKMUSQO" 
    --  changeThirdRotor 60 rotor1 rotor5 return Nothing  
    --  changeThirdRotor 61 rotor1 rotor5 return Nothing
    -- Failing Case: 
    --  changeThirdRotor 299 rotor1 rotor5 return Nothing   
    changeThirdRotor :: Int -> Rotor -> Rotor ->Maybe Rotor 
    changeThirdRotor count firstRotor secondRotor
        | (count >= 60) = Nothing
        | (thirdRotorCount == 0) = Just (listWithoutUsedRotor!!0)
        | (thirdRotorCount == 1) = Just (listWithoutUsedRotor!!1)
        | (thirdRotorCount == 2) = Just (listWithoutUsedRotor!!2)
        | otherwise = Nothing
        where 
            listWithoutUsedRotor = allRotorList \\ [firstRotor,secondRotor]    
            thirdRotorCount = count `mod` 3
        