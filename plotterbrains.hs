{-
                     \====\.                    ./===============\.                    ./====/
                     o\=====================================================================/o
                     ooo===================================================================ooo
                     \o|o|//'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''\\|o|o/
                     'o|o|/                                                             \|o|o'
                      o|o|     .                                                    .    |o|o
                     /o|o|                /----------------------------\                 |o|o\
                     |o|o|                |  --  plotterbrains.hs  --  |                 |o|o|
                     \o|o|                \----------------------------/                 |o|o/
                      o|o|     .                                                    .    |o|o
                     .o|o|\                                                             /|o|o.
                     /o|o|\\...........................................................//|o|o|
                     ooo===================================================================ooo
                     o/=====================================================================\o
                     /====/'                    '\===============/'                    '\====\

    plotterbrains.hs Version 1.0.0.0
-}

--                /------------------------------------------------------\
--                |------------------------------------------------------|
--                |--------------------  - Notes -  ---------------------|
--                |------------------------------------------------------|
--                \------------------------------------------------------/

-- Lorem something about see what for documentation and for test examples.

-- Rainbow number 1 is 11 colors of Minecraft wool.  Rainbow number 2 is 22 different kinds of Minecraft blocks including 11 colors of wool and 11 other block types (diamond, gold, mossy cobblestone, etc).  rainbow number 1 can be called with numbers up to 15 where the last 4 are black, white, and light and dark grey.  When using goJS with numbers that go up to 15, use rainbow number 3.




--                /------------------------------------------------------\
--                |------------------------------------------------------|
--                |-------------------  - Imports -  --------------------|
--                |------------------------------------------------------|
--                \------------------------------------------------------/

import Data.List (intercalate, nub, sortBy, zipWith4)
import System.Random (randomR, mkStdGen)





--                /------------------------------------------------------\
--                |------------------------------------------------------|
--                |---------------  - Type Definitions -  ---------------|
--                |------------------------------------------------------|
--                \------------------------------------------------------/

-- A point in 2D space is a pair of floats for x and y co-ordinates.
type Point2D = (Float, Float)

-- A point in 3D space is a triple of floats for x, y, and z co-ordinates.
type Point3D = (Float, Float, Float)

-- A vector in 2D space is a pair of floats for x and y components.
type Vector2D = (Float, Float)

-- A vector in 3D space is a triple of floats for x, y, and z components.
type Vector3D = (Float, Float, Float)

-- Note: For 2D scalar fields and 3D scalar fields, all the dimensions should be consistent, but there's no enforcement.  If the dimensions aren't consistent then it will maybe crash with out of bounds error and maybe not.  When functions do things to these, the dimensions are tested as length of first row first column.  If any of the lists (or lists of lists, or lists of lists of lists) after first are smaller than the first, then there will be an out of bounds error.  If any are bigger, then the extra values will be ignored and there won't be a crash.

-- A 2D scalar field is a 2D grid of scalar values.  This is represented as a list of lists.  Example: For temperature as a function of x and y, this data type would be a list of lists of temperature values.
type ScalarField2D = [[Float]]

-- A 3D scalar field is a 3D grid of scalar values.  This is represented as a list of lists of lists.  Example: For temperature as a function of x and y and z, this data type would be a list of lists of lists of temperature values.
type ScalarField3D = [[[Float]]]

-- A sliced 3D scalar field is a 3D grid of scalar values that represent a plot of fully dense slices.  Keep track of the slice direction when you use one of these.
type ScalarField3DSliced = [[[Float]]]

-- A U set is a set of co-ordinates plus an unscaled value.  Example: a set of (x, y, z) co-ordinates and the value of a 3D scalar field at that point.
type Uset = (Int, Int, Int, Float)

-- A U list is a list of U sets.
type Ulist = [Uset]

-- A P set ("set of information for a plot point") is a set of co-ordinates plus a color value.  These should be in Minecraft x, y, z, and the color value should be specific to whether you're using an 11-value rainbow or a 22-value rainbow.
type Pset = (Int, Int, Int, Int)

-- A P list is a list of P sets.  All of the P sets in a given P list should have their color values specific to whether you're using an 11-color rainbow or a 22-color rainbow.
type Plist = [Pset]




--                /------------------------------------------------------\
--                |------------------------------------------------------|
--                |-----------  - Simple Helper Functions -  ------------|
--                |------------------------------------------------------|
--                \------------------------------------------------------/

-- This moves all the blocks of a plot some number of grid spaces in the up (+y) direction.
movePListUp :: Plist -> Int -> Plist
movePListUp inpPList moveDist    = map (\(x, y, z, c) -> (x, y + moveDist, z, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the down (-y) direction.
movePListDown :: Plist -> Int -> Plist
movePListDown inpPList moveDist  = map (\(x, y, z, c) -> (x, y - moveDist, z, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the South (+z) direction.
movePListSouth :: Plist -> Int -> Plist
movePListSouth inpPList moveDist = map (\(x, y, z, c) -> (x, y, z + moveDist, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the East (+x) direction.
movePListEast :: Plist -> Int -> Plist
movePListEast inpPList moveDist  = map (\(x, y, z, c) -> (x + moveDist, y, z, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the North (-z) direction.
movePListNorth :: Plist -> Int -> Plist
movePListNorth inpPList moveDist = map (\(x, y, z, c) -> (x, y, z - moveDist, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the West (-x) direction.
movePListWest :: Plist -> Int -> Plist
movePListWest inpPList moveDist  = map (\(x, y, z, c) -> (x - moveDist, y, z, c)) inpPList


-- p list bounds returns 6 values representing the minimum and maximum x, y, and z values of some plot.
pListBounds :: Plist -> (Int, Int, Int, Int, Int, Int)
pListBounds inpPList = (xMin, xMax, yMin, yMax, zMin, zMax)
   where xMin = minimum (map (\(x, _, _, _) -> x) inpPList)
         xMax = maximum (map (\(x, _, _, _) -> x) inpPList)
         yMin = minimum (map (\(_, y, _, _) -> y) inpPList)
         yMax = maximum (map (\(_, y, _, _) -> y) inpPList)
         zMin = minimum (map (\(_, _, z, _) -> z) inpPList)
         zMax = maximum (map (\(_, _, z, _) -> z) inpPList)


-- rand colors generates an infinitely long list of random numbers selected as whole numbers between 1 and 15.
randColors :: [Int]
randColors = let randSeed = 69420 in map (\a -> fst (randomR (1 :: Int, 15) (mkStdGen $ a * randSeed))) [1 ..]

-- lorem
randColors22 :: [Int]
randColors22 = let randSeed = 69420 in map (\a -> fst (randomR (1 :: Int, 22) (mkStdGen $ a * randSeed))) [1 ..]


-- add 2D scalar fields takes a pair of 2D scalar fields and returns a scalar field whose values are sums of corresponding terms of the inputs.
add2DSF :: ScalarField2D -> ScalarField2D -> ScalarField2D
add2DSF inpSF1 inpSF2 = [ [ ((inpSF1 !! pred yInd) !! pred xInd) + ((inpSF2 !! pred yInd) !! pred xInd) | xInd <- xInds] | yInd <- yInds ]  -- For each of the x indices and for each of the y indices, the corresponding entry in the result is the sum of the values found at those indices in the two inputs.
   where yDim = length inpSF1  -- y dim is the number of points in the y direction, which is the second dimension of the 2D array.  This assumes the y dim of the second input will be equal to the y dim of the first.
         xDim = length $ head inpSF1  -- x dim is the number of points in the x direction, which is the first dimension of the 2D array.  This assumes all x dims (of all x-rows of both inputs) will be equal to the size of the first x-row of the first input.
         yInds = [1, 2 .. yDim]  -- The y indices range from 1 to the number of points in the y direction.
         xInds = [1, 2 .. xDim]  -- The x indices range from 1 to the number of points in the x direction.


-- add 3D scalar fields takes a pair of 3D scalar fields and returns a scalar field whose values are sums of corresponding terms of the inputs.
addsc3DFields :: ScalarField3D -> ScalarField3D -> ScalarField3D
addsc3DFields inpSF1 inpSF2 = [ [ [ (((inpSF1 !! pred zInd) !! pred yind) !! pred xInd) + (((inpSF2 !! pred zInd) !! pred yind) !! pred xInd) | xInd <- xInds] | yind <- yInds] | zInd <- zInds ]  -- For each of the x indices and for each of the y indices and for each of the z indices, the corresponding entry in the result is the sum of the values found at those indices in the two inputs.
   where zDim = length inpSF1  -- z dim is the number of points in the z direction, which is the third dimension of the 3D array.  This assumes the z dim of the second input will be equal to the z dim of the first.
         yDim = length $ head inpSF1  -- y dim is the number of points in the y direction, which is the second dimension of the 3D array.  This assumes all y dims (of all y-rows of both inputs) will be equal to the size of the first y-row of the first input.
         xDim = length $ (head . head) inpSF1  -- x dim is the number of points in the x direction, which is the first dimension of the 3D array.  This assumes all x dims (of all x-rows of both inputs) will be equal to the size of the first x-row of the first input.
         zInds = [1, 2 .. zDim]  -- The z indices range from 1 to the number of points in the z direction.
         yInds = [1, 2 .. yDim]  -- The y indices range from 1 to the number of points in the y direction.
         xInds = [1, 2 .. xDim]  -- The x indices range from 1 to the number of points in the x direction.


-- thicken takes a Plist and a number and thickens all parts of that plot by that amount.
thicken :: Plist -> Int -> Plist
thicken inpPList thickness = nub $ concatMap makeMore inpPList  -- The result is obtained by taking the input list, listing all points around all input list points to some (x, y, z) distances, flattening that list of lists to a list, and removing redundantly equivalent values.
   where offset = floor (fromIntegral thickness / 2.0)  -- The offset is half the thickness, used in the equations for resultant co-ordinates to make sure the thickening is centered around the input blocks.
         [dxs, dys, dzs] = replicate 3 [1 .. thickness]  -- The list of dx values is a list of numbers from 1 to the thickness value from the input.  Likewise dy and dz.
         makeMore (inp_x, inp_y, inp_z, inpC) = [(inp_x + dx - offset, inp_y + dy - offset, inp_z + dz - offset, inpC) | dx <- dxs, dy <- dys, dz <- dzs]  -- The list of blocks around a block is a block for each combination of dx and dy and dz, using offset to keep the output centered.


-- opacity takes a Plist and a number and randomly removes that percentage of blocks using seeded random number generation.
opacity :: Plist -> Int -> Plist
opacity inpPList opacity = map fst $ filter (\(_,b) -> b == True) (zip inpPList keepThrowList)  -- The result is obtained by taking some list of true and false values and keeping/removing input list values according to it.
   where numberifyBlock (inp_x, inp_y, inp_z, _) = inp_x * 1000000 + inp_y * 1000 + inp_z  -- This equation takes the x, y, and z values of a block and turns them into a single number for seeding the RNG.
         numberifiedList = map numberifyBlock inpPList  -- This is a list of RNG seeds corresponding to the list of input block co-ordinates.
         diceRollList = map (fst . randomR (1::Int,100) . mkStdGen) numberifiedList  -- This takes the list of RNG seeds and converts it into a list of values with each being the result of a 100-sided dice roll.
         keepThrowList = map (\a -> if opacity > a then True else False) diceRollList  -- This takes the list of 100-sided dice rolls, uses the opacity value from the input, and makes a list of True/False values for which of the input blocks to keep and which to filter out.

-- lorem
opacitywSeed :: Plist -> Int -> Int -> Plist
opacitywSeed inpPList opacity seedNum = map fst $ filter (\(_,b) -> b == True) (zip inpPList keepThrowList)  -- lorem
   where numberifyBlock (inp_x, inp_y, inp_z, _) = inp_x * 100000 * seedNum + inp_y * 100 * seedNum + inp_z  -- lorem
         numberifiedList = map numberifyBlock inpPList  -- lorem
         diceRollList = map (fst . randomR (1::Int,100) . mkStdGen) numberifiedList  -- lorem
         keepThrowList = map (\a -> if opacity > a then True else False) diceRollList  -- lorem


-- rgb max sat takes a hue number and returns a color with maximum saturation and brightness for that hue value.  The hue value should be between 0.0 and 360.0
rgbMaxSat :: Float -> (Int, Int, Int)
rgbMaxSat hue = resultColor
   where colorR = (255, 0  , 0  )  -- This is the RGB triple for red.
         colorG = (0  , 255, 0  )  -- This is the RGB triple for green.
         colorB = (0  , 0  , 255)  -- This is the RGB triple for blue.
         colorC = (0  , 255, 255)  -- This is the RGB triple for cyan.
         colorM = (255, 0  , 255)  -- This is the RGB triple for magenta.
         colorY = (255, 255, 0  )  -- This is the RGB triple for yellow.
         lerpColors (r1, g1, b1) (r2, g2, b2) howFar = (floor (fromIntegral r1 * (1.0 - howFar) + fromIntegral r2 * howFar),floor (fromIntegral g1 * (1.0 - howFar) + fromIntegral g2 * howFar), floor (fromIntegral b1 * (1.0 - howFar) + fromIntegral b2 * howFar))  -- This is the easy way of linearly interpolating between colors, which should actually work perfectly if interpolating between the 6 primary colors RGB and CMY.  The equation takes two colors and returns a color where each component of the resultant is a weighted average of the corresponding components of the two inputs, with the weight being the input howFar, which should always be normalized to between 0 and 1.
         resultColor  -- The resultant color will be a linear interpolation between two primary colors (one from RGB, one from CMY), and the choice of which two colors depends on which sector the hue is in when the color wheel is divided into 6 sectors.
           | hue > 300.0 = lerpColors colorM colorR ((hue - 300.0) / 60.0)  -- If the hue number is in sector 6 of 6 of the color wheel, then the resultant color is an interpolation between magenta and red.
           | hue > 240.0 = lerpColors colorB colorM ((hue - 240.0) / 60.0)  -- If the hue number is in sector 5 of 6 of the color wheel, then the resultant color is an interpolation between blue and magenta.
           | hue > 180.0 = lerpColors colorC colorB ((hue - 180.0) / 60.0)  -- If the hue number is in sector 4 of 6 of the color wheel, then the resultant color is an interpolation between cyan and blue.
           | hue > 120.0 = lerpColors colorG colorC ((hue - 120.0) / 60.0)  -- If the hue number is in sector 3 of 6 of the color wheel, then the resultant color is an interpolation between green and cyan.
           | hue > 60.0  = lerpColors colorY colorG ((hue -  60.0) / 60.0)  -- If the hue number is in sector 2 of 6 of the color wheel, then the resultant color is an interpolation between yellow and green.
           | otherwise   = lerpColors colorR colorY           (hue / 60.0)  -- If the hue number is in sector 1 of 6 of the color wheel, then the resultant color is an interpolation between red and yellow.


-- theta array makes a circular array of 2D points around the origin.  Give it a radius and a number of points, and it gives you a list of (x, y) points.  Use some other function to move it from the center to wherever you want to put it.
thetaArray :: Float  -- ^radius (rho)
  -> Int  -- ^number of theta values (polar angles)
  -> [Point2D]
thetaArray rho numThetas = [let theta = thetas !! pred thetaInd in (x rho theta, y rho theta) | thetaInd <- thetaInds]  -- The list of all point (x, y) values is obtained by taking all theta indices and then for each one calculating the x and y values from that theta and the radius.
   where thetaInds = [1 .. numThetas]  -- Theta indices range from 1 to the input number of points.
         x rho theta = rho * sin theta  -- This is the formula to get an x value from a pair of polar co-ordinates.
         y rho theta = rho * cos theta  -- This is the formula to get a y valye from a pair of polar co-ordinates.
         thetas = map indexToAngle [1 .. numThetas]  -- The theta values are obtained by mapping some function to the list of indices (first theta, second theta, etc).
            where indexToAngle ind = 2.0 * pi * (fromIntegral ind - 1.0) / fromIntegral numThetas  -- This is the function for taking an index number for a theta and getting the theta value in radians.


-- lorem
hThetaArray :: Float  -- ^lorem
  -> Float  -- ^lorem
  -> Int  -- ^lorem
  -> Int  -- ^lorem
  -> [Point3D]
hThetaArray rho maxH numThetas numHs = [let theta = thetas !! pred thetaInd in (x rho theta, y rho theta, hs !! pred hInd) | thetaInd <- thetaInds, hInd <- hInds]  -- lorem
   where thetaInds = [1 .. numThetas]  -- lorem
         hInds = [1 .. numHs]  -- lorem
         hStep = maxH / fromIntegral (pred numHs)  -- lorem
         x rho theta = rho * sin theta  -- lorem
         y rho theta = rho * cos theta  -- lorem
         thetas = map indexToAngle [1 .. numThetas]  -- lorem
            where indexToAngle ind = 2.0 * pi * (fromIntegral ind - 1.0) / fromIntegral numThetas  -- lorem
         hs = map ((hStep *) . fromIntegral . pred) hInds  -- lorem


-- phi theta array makes a spherical array of 3D points around the origin.  Give it a radius, a number of lines of latitude, a number of lines of longitude, and a boolean condition of whether or not to include the z-pole points, and it gives you a list of (x, y, z) points.  Use some other function to move it from the center to wherever you want to put it.
phiThetaArray :: Float  -- ^radius (rho)
  -> Int  -- ^number of phi values (elevation angles)
  -> Int  -- ^number of theta values (yaw angles)
  -> Bool  -- ^include poles in result?
  -> [Point3D]
phiThetaArray rho numPhis numThetas polesCondition = if polesCondition then concat [[pole1], nonPoleList, [pole2]] else nonPoleList  -- If the bool is true, then the results includes the pole points.  If false, then not.
   where pole1 = (0.0, 0.0, rho)  -- (x, y, z) of pole 1.
         pole2 = (0.0, 0.0, negate rho)  -- (x, y, z) of pole 2.
         numNonPolarPhis = numPhis - 2  -- The number of phi values that aren't at the poles.
         nonPolarPhis = map indexToAngle [1 .. numNonPolarPhis]  -- The phi values (list of values in radians) are obtained by mapping some function to the list of indices (first phi, second phi, etc).
            where indexToAngle ind = (pi * fromIntegral ind) / (fromIntegral numPhis - 1.0)  -- This is the function for taking an index number for a phi and getting the phi value in radians.
         thetas = map indexToAngle [1 .. numThetas]  -- The theta values (list of values in radians) are obtained by mapping some function to the list of indices (first theta, second theta, etc).
            where indexToAngle ind = 2.0 * pi * (fromIntegral ind - 1.0) / fromIntegral numThetas  -- This is the function for taking an index number for a theta and getting the theta value in radians.
         nonPoleList = [let phi = nonPolarPhis !! pred phiInd; theta = thetas !! pred thetaInd in (x rho phi theta, y rho phi theta, z rho phi theta) | phiInd <- phiInds, thetaInd <- thetaInds]  -- The list of all point (x, y, z) values except for the polar ones is obtained by taking all phi and theta values and making a list of (x, y, z) values corresponding to all combinations of those angle values, calculating the values of x, y, and z using the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
            where phiInds = [1 .. numNonPolarPhis]  -- phi index values start at 1 and end at a number two less than the number of phis from the input.
                  thetaInds = [1 .. numThetas]  -- theta index values start at 1 and end at the number of thetas from the input.
                  x rho phi theta = rho * sin phi * cos theta  -- This is the x component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                  y rho phi theta = rho * sin phi * sin theta  -- This is the y component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                  z rho phi theta = rho * cos phi  -- This is the z component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).




--                /------------------------------------------------------\
--                |------------------------------------------------------|
--                |--------------  - Plotting Functions -  --------------|
--                |------------------------------------------------------|
--                \------------------------------------------------------/

--   -------- Subsection of plotting functions: before getting to U lists --------
--   -----------------------------------------------------------------------------

-- fill 2D scalar field is the function for getting a 2D array of scalar values from a range of min and max x and y values, a number of slices to take in those ranges of x and y, and a function that returns a scalar value from an x and y pair.
fill2DSF :: (Float -> Float -> Float)  -- ^a scalar field equation for getting a scalar value from an (x, y) pair
  -> Int  -- ^number of points in x direction
  -> Int  -- ^number of points in y direction
  -> Float  -- ^minimum x value
  -> Float  -- ^maximum x value
  -> Float  -- ^minimum y value
  -> Float  -- ^maximum y value
  -> ScalarField2D
fill2DSF sfEqa xPts yPts xMin xMax yMin yMax = [ [ sfEqa (x m) (y n) | m <- ms ] | n <- ns ]  -- This gets the resulting 2D array of values by applying the scalar field equation to the list of all combinations of x for m value (x index) and y for n value (y index).
   where ms = [1, 2 .. xPts]  -- This is the list of m values, which are the indices for the x direction.
         ns = [1, 2 .. yPts]  -- This is the list of n values, which are the indices for the y direction.         
         xRange = xMax - xMin  -- x range is the value of the span of x values.
         yRange = yMax - yMin  -- y range is the value of the span of y values.
         dx = xRange / fromIntegral (pred xPts)  -- dx is the step size between successive x values.
         dy = yRange / fromIntegral (pred yPts)  -- dy is the step size between successive y values.
         x m = xMin + fromIntegral (pred m) * dx  -- This gives an x value for an x index (n) based on the range and step size for x.
         y n = yMin + fromIntegral (pred n) * dx  -- This gives a y value for a y index (m) based on the range and step size for x.


-- equipotential curve(s) in 2D returns a P list for one or any number of equipotential curves.  Takes one or a list of conditions (potential, tolerance, and color for each curve), a 2D scalar field equation, max bounds for x and z Minecraft co-ordinates (min for both are hard-coded to start at 1 and height is hard-coded as 100), and min and max values for x and y in equation space.  Note: before this step, we formulate the equation in terms of x and y, but then at this step, we convert (x, y, _) to (z, x, _) (because Minecraft has y for vertical).
eqPoCrvs2D :: (Float -> Float -> Float)  -- ^a scalar field equation for getting a scalar value from an (x, y) pair
  -> [(Float, Float, Int)]  -- ^list of conditions for each curve.  Each curve is defined by a value for potential, a value for tolerance, and a value for color
  -> Int  -- ^Minecraft x dimension max value
  -> Int  -- ^Minecraft z dimension max value
  -> Float  -- ^equation space x dimension min value
  -> Float  -- ^equation space x dimension max value
  -> Float  -- ^equation space y dimension min value
  -> Float  -- ^equation space y dimension max value
  -> Plist
eqPoCrvs2D sfEqa inpList mcxMax mczMax eqxMin eqxMax eqyMin eqyMax = map (\(Just a) -> a) justsAllCrvs  -- This takes all the P sets for all curves that weren't Nothings, and extracts all the values from their Justs.
   where getMaybesOneCrv (potential, tolerance, color) = [ if inBounds (x mcz) (y mcx) then Just (mcx, 101, mcz, color) else Nothing | mcx <- mcxs, mcz <- mczs ]  -- This takes the potential, tolerance, and color for one curve and returns P sets for all blocks that should be part of it.
            where mczs = [1 .. mczMax]  -- Minecraft z values range from 1 to input max z value.
                  mcxs = [1 .. mcxMax]  -- Minecraft x values range from 1 to input max x value.
                  x mcz = fromIntegral mcz * (eqxMax - eqxMin) / fromIntegral mczMax + eqxMin  -- This takes a Minecraft z value and maps it to the x in the equation space.
                  y mcx = fromIntegral mcx * (eqyMax - eqyMin) / fromIntegral mcxMax + eqyMin  -- This takes a Minecraft x value and maps it to the y in the equation space.
                  difference x z = sfEqa x z - potential  -- This takes an x and z in equation space, finds the scalar field value there, and compares it to the target value for one curve.
                  inBounds x z = abs (difference x z) < tolerance  -- This takes an x and z in equation space, and finds whether the scalar field value there is within the tolerance of the target value for one curve.
         maybesAllCrvs = concatMap getMaybesOneCrv inpList  -- This takes the function that returns P sets for one curve and applies it to all the curves from the input.
         justsAllCrvs = filter (/= Nothing) maybesAllCrvs  -- This takes the P sets for all curves and filters out all the Nothings.


-- field lines 2D returns a list of Minecraft block locations and colors based on stepping along a gradient function in 2D.  The output should plot as a set of curves.  It takes an equation for computing a 2D gradient, and a list of starting locations and colors, and generates the curves by stepping along that gradient to turn each starting location into a curve.  It needs values for converting from the equation space to the Minecraft space (scales and offsets), a condition to determine how to cut off a curve, a step size, and a fake starting magnitude for each curve.
fieldLines2D :: (Point2D -> Vector2D)  -- ^an equation that takes a 2D point and returns a 2D vector for the gradient there
  -> [(Point2D, Int)]  -- ^the input list of 2D starting points, each of which will become a curve, and the color for each curve / starting point
  -> Float  -- ^the factor for converting from equation space x to Minecraft space z
  -> Float  -- ^the factor for converting from equation space y to Minecraft space x
  -> Float  -- ^offset value for comparing the equation space's origin x to the Minecraft space's origin z
  -> Float  -- ^offset value for comparing the equation space's origin y to the Minecraft space's origin x
  -> ((Float, Float, Float) -> Bool)  -- ^a condition that takes a 2D point and magnitude and determines whether it's not beyond the threshold for keeping (e.g. magnitude is low enough and point is within the graph)
  -> Float  -- ^a fake starting magnitude to pair with each curve's starting point
  -> Float  -- ^the step size in Minecraft meters
  -> Bool  -- ^a condition for reversing the polarity or not
  -> Plist
fieldLines2D gradEq inpList scale_dim1 scale_dim2 offset_dim1 offset_dim2 keepCond fakeStartMag stepSize reverseYN = thicken (concatMap plist1c inpListwFakeStartMags) 2  -- This takes all the tuples for a curve's starting location, color, and fake start magnitude, computes the Minecraft block locations and colors for each, flattens the results, and thickens those.
   where inpListwFakeStartMags = map (\((x, y), c) -> ((x, y, fakeStartMag), c)) inpList  -- This takes the input start points and colors and adds the fake starting magnitude to each tuple.
         nextLoc (xi, yi, mi) = if not reverseYN then (xi + dx, yi + dy, magGrad) else (xi - dx, yi - dy, magGrad)  -- This takes a set of values for x and y and magnitude there and generates the x and y and magnitude of the next point.
            where (grad_x, grad_y) = gradEq (xi, yi)  -- This returns the x and y components of the 2D gradient at a point.
                  magGrad = sqrt (grad_x ** 2.0 + grad_y ** 2.0)  -- This returns the magnitude of the 2D gradient at a point.
                  dx = grad_x / magGrad * stepSize  -- This returns the step size in the equation x direction by normalizing the gradient, multiplying that unit vector by the step size, and taking the x component.
                  dy = grad_y / magGrad * stepSize  -- This returns the step size in the equation y direction by normalizing the gradient, multiplying that unit vector by the step size, and taking the y component.
         infinLocs1c startLoc = iterate nextLoc startLoc  -- This returns an infinitely long list of locations by taking some starting location and taking the next step an infinite number of times.
         keeperLocs1c startLoc = takeWhile keepCond (infinLocs1c startLoc)  -- This returns a finitely long list of locations by taking some staring location, generating an infinitely long list of next step values, and then cutting off the list after the keep condition from the input.
         eq_xy_to_MC_xyz (ex, ey, _) = (mcx, mcy, mcz)  -- This maps a point in the equation (x, y) to Minecraft (x, y, z) (by (x, y, _) -> (z, x, y)), using a MC y height of 100 for a level plane.
            where mcx = round ((ey + offset_dim2) * scale_dim2)  -- In this conversion process, a Minecraft x value is based on an equation y value and the dim 2 offset and scale values from the input.
                  mcy = 101  -- The altitude of level plane in Minecraft y.
                  mcz = round ((ex + offset_dim1) * scale_dim1)  -- In this conversion process, a Minecraft z value is based on an equation x value and the dim 1 offset and scale values from the input.
         mckeeperLocs1c startLoc = nub $ map eq_xy_to_MC_xyz $ keeperLocs1c startLoc  -- This takes a starting location, gets the list of points that satisfy the keep condition based on stepping from there, converts to Minecraft space, and removes duplicates.
         plist1c (startLoc, color) = map (\(x, y, z) -> (x, y, z, color)) $ mckeeperLocs1c startLoc  -- This takes the list of Minecraft block locations (with duplicates removed) and tuples the color for that curve with each block.


-- lorem
vectorField2DLinearScale :: (Point2D -> Vector2D)  -- ^an equation that takes a 2D point and returns a 2D vector for the gradient there
  -> Int  -- ^lorem
  -> Int  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Int  -- ^lorem
  -> Int  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Int  -- ^lorem
  -> Plist
vectorField2DLinearScale gradEq mcxMax mczMax eqxMin eqxMax eqyMin eqyMax numPoints_x numPoints_y arrowMin arrowMax bulbSize = concatMap (\vs -> rainbowify vs ++ plotDotify vs) gridNormGradsLens  -- lorem
   where xInds = [1 .. numPoints_x]  -- lorem
         yInds = [1 .. numPoints_y]  -- lorem
         xs = map (\x -> eqxMin + (eqxMax - eqxMin) * fromIntegral (pred x) / fromIntegral (pred numPoints_x)) xInds  -- lorem
         ys = map (\y -> eqyMin + (eqyMax - eqyMin) * fromIntegral (pred y) / fromIntegral (pred numPoints_y)) yInds  -- lorem
         gridPoints = [(x, y) | x <- xs, y <- ys ]  -- lorem
         gridNormGradsMags = map (\p@(x, y) -> let (gx, gy) = gradEq p; mag = sqrt(gx ^ 2 + gy ^ 2) in (x, y, gx / mag, gy / mag, mag)) gridPoints  -- lorem
         gridNormGradsMagsSorted = sortBy (\(_, _, _, _, mag1) (_, _, _, _, mag2) -> compare mag1 mag2) gridNormGradsMags  -- lorem
         arrow_dr = (arrowMax - arrowMin) / fromIntegral (length gridPoints)  -- lorem
         arrowLengths = [arrowMin, arrowMin + arrow_dr ..]  -- lorem
         gridNormGradsLens = zipWith (\(x, y, ngx, ngy, _) len -> (x, y, ngx, ngy, len)) gridNormGradsMagsSorted arrowLengths  -- lorem
         rainbowify (x1, y1, ngx, ngy, len) = rainbowLine (x1, y1, 20.0) (x2, y2, 20.0) mcxMax 100 mczMax eqxMin eqxMax eqyMin eqyMax 10.0 20.0 numEachColor -- lorem
            where xLen = ngx * len -- lorem
                  yLen = ngy * len -- lorem
                  x2 = x1 + xLen -- lorem
                  y2 = y1 + yLen -- lorem
                  mczLen = xLen / (eqxMax - eqxMin) * fromIntegral mczMax -- lorem
                  mcxLen = yLen / (eqyMax - eqyMin) * fromIntegral mcxMax -- lorem
                  mcLen = sqrt(mczLen ^ 2 + mcxLen ^ 2) -- lorem
                  numEachColor = ceiling (mcLen / 8.0) -- lorem
         plotDotify (x, y, ngx, ngy, len) = plotDot (x, y, 20.0) mcxMax 100 mczMax eqxMin eqxMax eqyMin eqyMax 10.0 20.0 bulbSize 1 -- lorem


-- lorem
rainbowLine :: Point3D -> Point3D -> Int -> Int -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> Int -> Plist
rainbowLine (x1, y1, z1) (x2, y2, z2) mcxMax mcyMax mczMax eqxMin eqxMax eqyMin eqyMax eqzMin eqzMax numEachColor = zipWith4 zipFunction xs ys zs colorList
   where dist = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)  -- lorem
         colorList = concatMap (replicate numEachColor) [1 .. 11]  -- lorem
         numPoints = length colorList  -- lorem
         dx = (x2 - x1) / fromIntegral numPoints  -- lorem
         dy = (y2 - y1) / fromIntegral numPoints  -- lorem
         dz = (z2 - z1) / fromIntegral numPoints  -- lorem
         xs = [x1, x1 + dx ..]  -- lorem
         ys = [y1, y1 + dy ..]  -- lorem
         zs = [z1, z1 + dz ..]  -- lorem
         zipFunction x y z c = (mcx, mcy, mcz, c)  -- lorem
            where mcx = round (fromIntegral mcxMax * (y - eqyMin) / (eqyMax - eqyMin))  -- lorem
                  mcy = round (fromIntegral mcyMax * (z - eqzMin) / (eqzMax - eqzMin))  -- lorem
                  mcz = round (fromIntegral mczMax * (x - eqxMin) / (eqxMax - eqxMin))  -- lorem


-- lorem
plotDot :: Point3D -> Int -> Int -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> Int -> Int -> Plist
plotDot (eqx, eqy, eqz) mcxMax mcyMax mczMax eqxMin eqxMax eqyMin eqyMax eqzMin eqzMax size colorNum = thicken [(mcx, mcy, mcz, colorNum)] size
   where mcx = round (fromIntegral mcxMax * (eqy - eqyMin) / (eqyMax - eqyMin))  -- lorem
         mcy = round (fromIntegral mcyMax * (eqz - eqzMin) / (eqzMax - eqzMin))  -- lorem
         mcz = round (fromIntegral mczMax * (eqx - eqxMin) / (eqxMax - eqxMin))  -- lorem


-- fill 3D scalar field is the function for getting a 3D array of scalar values from a 3D scalar field equation where the resulting array is to represent an evenly spaced grid of points throughout the field.  It takes a 3D scalar field equation, a number of points to take in the x, y, and z directions of the equation space, and minimum and maximum values for x, y, and z.
fill3DSF :: (Float -> Float -> Float -> Float)  -- ^a scalar field equation for getting a scalar value from an (x, y, z) triple.
  -> Int  -- ^number of points in the x direction
  -> Int  -- ^number of points in the y direction
  -> Int  -- ^number of points in the z direction
  -> Float  -- ^minimum x value
  -> Float  -- ^maximum x value
  -> Float  -- ^minimum y value
  -> Float  -- ^maximum y value
  -> Float  -- ^minimum z value
  -> Float  -- ^maximum z value
  -> ScalarField3D
fill3DSF sfEqa xPts yPts zpts xMin xMax yMin yMax zMin zMax = [ [ [ sfEqa (x m) (y n) (z o) | m <- ms ] | n <- ns ] | o <- os ]  -- This gets the resulting 3D array of values by applying the scalar field equation to the list of all combinations of x for m value (x index), y for n value (y index), and z for o value (z index).
   where ms = [1, 2 .. xPts]  -- This is the list of m values, which are the indices for the x direction.
         ns = [1, 2 .. yPts]  -- This is the list of n values, which are the indices for the y direction.
         os = [1, 2 .. zpts]  -- This is the list of o values, which are the indices for the z direction.
         xRange = xMax - xMin  -- x range is the value of the span of x values.
         yRange = yMax - yMin  -- y range is the value of the span of y values.
         zrange = zMax - zMin  -- z range is the value of the span of z values.
         dx = xRange / fromIntegral (pred xPts)  -- dx is the step size between successive x values.
         dy = yRange / fromIntegral (pred yPts)  -- dy is the step size between successive y values.
         dz = zrange / fromIntegral (pred zpts)  -- dz is the step size between successive z values.
         x m = xMin + fromIntegral (pred m) * dx  -- This gives an x value for an x index (m) based on the range and step size for x.
         y n = yMin + fromIntegral (pred n) * dy  -- This gives a y value for a y index (n) based on the range and step size for y.
         z o = zMin + fromIntegral (pred o) * dz  -- This gives a z value for a z index (o) based on the range and step size for z.


-- fill 3D scalar field sliced is the function for getting a 3D array of scalar values from a 3D scalar field equation where the resulting array is to represent all the points on a series of slices through the field.  It takes a 3D scalar field equation, a slice direction (1 for x, 2 for y, 3 for z), plot width in the other two perpendicular directions, and minimum and maximum values for x, y, and z in the equation space.
fill3DSFSliced :: (Float -> Float -> Float -> Float)  -- ^a scalar field equation for getting a scalar value from an (x, y, z) triple.
  -> Int  -- ^slice direction number: 1 for x, 2 for y, 3 for z (in equation space)
  -> Int  -- ^number of slices
  -> Int  -- ^width of slice in direction of dimension number 1 after slice direction
  -> Int  -- ^width of slice in direction of dimension number 2 after slice direction
  -> Float  -- ^minimum x value
  -> Float  -- ^maximum x value
  -> Float  -- ^minimum y value
  -> Float  -- ^maximum y value
  -> Float  -- ^minimum z value
  -> Float  -- ^maximum z value
  -> ScalarField3DSliced
fill3DSFSliced sfEqa sliceDirection numSlices dim2Pts dim3Pts xMin xMax yMin yMax zMin zMax = [ [ [ sfEqa (x m) (y n) (z o) | m <- ms ] | n <- ns ] | o <- os ]  -- This gets the resulting 3D array of values by applying the scalar field equation to the list of all combinations of x for m value (x index), y for n value (y index), and z for o value (z index).
   where (ms, ns, os) = let sliceInds = [1 .. numSlices]; dim2Inds = [1 .. dim2Pts]; dim3Inds = [1 .. dim3Pts] in case sliceDirection of  -- ms, ns, os are indices relative to equation x, equation y, equation z, respectively.  This conditional determines the number of indices in each of those directions.
                                       1 -> (sliceInds, dim2Inds, dim3Inds)
                                       2 -> (dim3Inds, sliceInds, dim2Inds)
                                       3 -> (dim2Inds, dim3Inds, sliceInds)
                                       _ -> undefined
         x m = case sliceDirection of  -- This conditional determines how to convert m indices into equation x values, which depends on the slice direction.  If the slice direction is 1 (slices are x slices), then the amount of x to step for each pair of successive indices is equal to the pitch between the slices.  If the slice direction is 2 or 3 (slices are y or z slices), then the amount of x to step for each pair of successive indices is whatever ends up mapping to one block length.
                      1 -> let slicePitch = (xMax - xMin) / fromIntegral (pred numSlices) in xMin + (fromIntegral (pred m) * slicePitch)
                      2 -> xMin + fromIntegral (pred m) * ((xMax - xMin) / fromIntegral (pred dim3Pts))
                      3 -> xMin + fromIntegral (pred m) * ((xMax - xMin) / fromIntegral (pred dim2Pts))
                      _ -> undefined
         y n = case sliceDirection of  -- This conditional determines how to convert n indices into equation y values, which depends on the slice direction.  If the slice direction is 2 (slices are y slices), then the amount of y to step for each pair of successive indices is equal to the pitch between the slices.  If the slice direction is 1 or 3 (slices are x or z slices), then the amount of y to step for each pair of successive indices is whatever ends up mapping to one block length.
                      1 -> yMin + fromIntegral (pred n) * ((yMax - yMin) / fromIntegral (pred dim2Pts))
                      2 -> let slicePitch = (yMax - yMin) / fromIntegral (pred numSlices) in yMin + (fromIntegral (pred n) * slicePitch)
                      3 -> yMin + fromIntegral (pred n) * ((yMax - yMin) / fromIntegral (pred dim3Pts))
                      _ -> undefined
         z o = case sliceDirection of  -- This conditional determines how to convert o indices into equation z values, which depends on the slice direction.  If the slice direction is 3 (slices are z slices), then the amount of z to step for each pair of successive indices is equal to the pitch between the slices.  If the slice direction is 1 or 2 (slices are x or y slices), then the amount of z to step for each pair of successive indices is whatever ends up mapping to one block length.
                      1 -> zMin + fromIntegral (pred o) * ((zMax - zMin) / fromIntegral (pred dim3Pts))
                      2 -> zMin + fromIntegral (pred o) * ((zMax - zMin) / fromIntegral (pred dim2Pts))
                      3 -> let slicePitch = (zMax - zMin) / fromIntegral (pred numSlices) in zMin + (fromIntegral (pred o) * slicePitch)
                      _ -> undefined


-- equipotential surfaces(s) in 3D returns a P list for one or any number of equipotential curves.  Takes one or a list of conditions (potential, tolerance, and color for each curve), a 3D scalar field equation, max bounds for x, y, and z Minecraft co-ordinates (min for all are hard-coded to start at 1), and min and max values for x, y, and z in equation space.  Note: before this step, we formulate the equation in terms of (x, y, z), but then at this step, we convert (x, y, z) to (z, x, y) (because Minecraft has y for vertical).
eqpSurfs3D :: (Float -> Float -> Float -> Float)  -- ^an equation that takes a 3D point and returns a 3D vector for the gradient there
  -> [(Float, Float, Int)]  -- ^list of conditions for each surface.  Each surface is defined by a value for potential, a value for tolerance, and a value for color
  -> Int  -- ^Minecraft x dimension max value
  -> Int  -- ^Minecraft y dimension max value
  -> Int  -- ^Minecraft z dimension max value
  -> Float  -- ^equation space x dimension min value
  -> Float  -- ^equation space x dimension max value
  -> Float  -- ^equation space y dimension min value
  -> Float  -- ^equation space y dimension max value
  -> Float  -- ^equation space z dimension min value
  -> Float  -- ^equation space z dimension max value
  -> Plist
eqpSurfs3D sfEqa inpList mcxMax mcyMax mczMax eqxMin eqxMax eqyMin eqyMax eqzMin eqzMax = map (\(Just a) -> a) justsAllSurfs  -- This takes all the P sets for all surfaces that weren't Nothings, and extracts all the values from their Justs.
   where getMaybesOneSurf (potential, tolerance, color) = [ if inBounds (x mcz) (y mcx) (z mcy) then Just (mcx, mcy, mcz, color) else Nothing | mcx <- mcxs, mcy <- mcys, mcz <- mczs ]  -- This takes the potential, tolerance, and color for one surface and returns P sets for all blocks that should be part of it.
            where mcxs = [1 .. mcxMax]  -- Minecraft x values range from 1 to input max x value.
                  mcys = [1 .. mcyMax]  -- Minecraft y values range from 1 to input max y value.
                  mczs = [1 .. mczMax]  -- Minecraft z values range from 1 to input max z value.
                  x mcz = fromIntegral mcz * (eqxMax - eqxMin) / fromIntegral mczMax + eqxMin  -- This takes a Minecraft z value and maps it to the x in the equation space.
                  y mcx = fromIntegral mcx * (eqyMax - eqyMin) / fromIntegral mcxMax + eqyMin  -- This takes a Minecraft x value and maps it to the y in the equation space.
                  z mcy = fromIntegral mcy * (eqzMax - eqzMin) / fromIntegral mcyMax + eqzMin  -- This takes a Minecraft y value and maps it to the z in the equation space.
                  difference x y z = sfEqa x y z - potential  -- This takes an x, y, and z in equation space, finds the scalar field value there, and compares it to the target value for one curve.
                  inBounds x y z = abs (difference x y z) < tolerance  -- This takes an x, y, and z in equation space, and finds whether the scalar field value there is within the tolerance of the target value for one curve.
         maybesAllSurfs = concatMap getMaybesOneSurf inpList  -- This takes the function that returns P sets for one surface and applies it to all the surfaces from the input.
         justsAllSurfs = filter (/= Nothing) maybesAllSurfs  -- This takes the P sets for all surfaces and filters out all the Nothings.


-- field lines 3D returns a list of Minecraft block locations and colors based on stepping along a gradient function in 3D.  The output should plot as a set of curves.  It takes an equation for computing a 3D gradient, and a list of starting locations and colors, and generates the curves by stepping along that gradient to turn each starting location into a curve.  It needs values for converting from the equation space to the Minecraft space (scales and offsets), a condition to determine how to cut off a curve, a step size, and a fake starting magnitude for each curve.
fieldLines3D :: (Point3D -> Vector3D)  -- ^an equation that takes a 3D point and returns a 3D vector for the gradient there
  -> [(Point3D, Int)]  -- ^the input list of 3D starting points, each of which will become a curve, and the color for each curve / starting point
  -> Float  -- ^the factor for converting from equation space x to Minecraft space z
  -> Float  -- ^the factor for converting from equation space y to Minecraft space x
  -> Float  -- ^the factor for converting from equation space z to Minecraft space y
  -> Float  -- ^offset value for comparing the equation space's origin x to the Minecraft space's origin z
  -> Float  -- ^offset value for comparing the equation space's origin y to the Minecraft space's origin x
  -> Float  -- ^offset value for comparing the equation space's origin z to the Minecraft space's origin y
  -> ((Float, Float, Float, Float) -> Bool)  -- ^a condition that takes a 3D point and magnitude and determines whether it's not beyond the threshold for keeping (e.g. magnitude is low enough and point is within the graph)
  -> Float  -- ^a fake starting magnitude to pair with each curve's starting point
  -> Float  -- ^the step size in Minecraft meters
  -> Bool  -- ^a condition for reversing the polarity or not
  -> Plist
fieldLines3D gradEq inpList scale_dim1 scale_dim2 scale_dim3 offset_dim1 offset_dim2 offset_dim3 keepCond fakeStartMag stepSize reverseYN = thicken (concatMap plist1c inpListwFakeStartMags) 2  -- This takes all the tuples for a curve's starting location, color, and fake start magnitude, computes the Minecraft block locations and colors for each, flattens the results, and thickens those.
   where inpListwFakeStartMags = map (\((x, y, z), c) -> ((x, y, z, fakeStartMag), c)) inpList  -- This takes the input start points and colors and adds the fake starting magnitude to each tuple.
         nextLoc (xi, yi, zi, mi) = if not reverseYN then (xi + dx, yi + dy, zi + dz, magGrad) else (xi - dx, yi - dy, zi - dz, magGrad)  -- This takes a set of values for x, y, z, and magnitude there and generates the x, y, z, and magnitude of the next point.
            where (grad_x, grad_y, grad_z) = gradEq (xi, yi, zi)  -- This returns the x, y, and z components of the 3D gradient at a point.
                  magGrad = sqrt(grad_x ** 2.0 + grad_y ** 2.0 + grad_z ** 2.0)  -- This returns the magnitude of the 3D gradient at a point.
                  dx = grad_x / magGrad * stepSize  -- This returns the step size in the equation x direction by normalizing the gradient, multiplying that unit vector by the step size, and taking the x component.
                  dy = grad_y / magGrad * stepSize  -- This returns the step size in the equation y direction by normalizing the gradient, multiplying that unit vector by the step size, and taking the y component.
                  dz = grad_z / magGrad * stepSize  -- This returns the step size in the equation z direction by normalizing the gradient, multiplying that unit vector by the step size, and taking the z component.
         infinLocs1c startLoc = iterate nextLoc startLoc  -- This returns an infinitely long list of locations by taking some starting location and taking the next step an infinite number of times.
         keeperLocs1c startLoc = takeWhile keepCond (infinLocs1c startLoc)  -- This returns a finitely long list of locations by taking some staring location, generating an infinitely long list of next step values, and then cutting off the list after the keep condition from the input.
         eq_xyz_to_mc_xyz (ex, ey, ez, m) = (mcx, mcy, mcz)  -- This maps a point in the equation (x, y, z) to Minecraft (x, y, z) (by (x, y, z) -> (z, x, y)).
            where mcx = floor ((ey + offset_dim2) * scale_dim2)  -- In this conversion process, a Minecraft x value is based on an equation y value and the dim 2 offset and scale values from the input.
                  mcy = floor ((ez + offset_dim3) * scale_dim3)  -- In this conversion process, a Minecraft y value is based on an equation z value and the dim 3 offset and scale values from the input.
                  mcz = floor ((ex + offset_dim1) * scale_dim1)  -- In this conversion process, a Minecraft z value is based on an equation x value and the dim 1 offset and scale values from the input.
         mckeeperLocs1c startLoc = nub $ map eq_xyz_to_mc_xyz $ keeperLocs1c startLoc  -- This takes a starting location, gets the list of points that satisfy the keep condition based on stepping from there, converts to Minecraft space, and removes duplicates.
         plist1c (startLoc, color) = map (\(x, y, z) -> (x, y, z, color)) (mckeeperLocs1c startLoc)  -- This takes the list of Minecraft block locations (with duplicates removed) and tuples the color for that curve with each block.


-- lorem
vectorField3DLinearScale :: (Point3D -> Vector3D)  -- ^an equation that takes a 2D point and returns a 2D vector for the gradient there
  -> Int  -- ^lorem
  -> Int  -- ^lorem
  -> Int  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Int  -- ^lorem
  -> Int  -- ^lorem
  -> Int  -- ^lorem
  -> Float  -- ^lorem
  -> Float  -- ^lorem
  -> Int  -- ^lorem
  -> Plist
vectorField3DLinearScale gradEq mcxMax mcyMax mczMax eqxMin eqxMax eqyMin eqyMax eqzMin eqzMax numPoints_x numPoints_y numPoints_z arrowMin arrowMax bulbSize = concatMap (\vs -> rainbowify vs ++ plotDotify vs) gridNormGradsLens  -- lorem
   where xInds = [1 .. numPoints_x]  -- lorem
         yInds = [1 .. numPoints_y]  -- lorem
         zInds = [1 .. numPoints_z]  -- lorem
         xs = map (\x -> eqxMin + (eqxMax - eqxMin) * fromIntegral (pred x) / fromIntegral (pred numPoints_x)) xInds  -- lorem
         ys = map (\y -> eqyMin + (eqyMax - eqyMin) * fromIntegral (pred y) / fromIntegral (pred numPoints_y)) yInds  -- lorem
         zs = map (\z -> eqzMin + (eqzMax - eqzMin) * fromIntegral (pred z) / fromIntegral (pred numPoints_z)) zInds  -- lorem
         gridPoints = [(x, y, z) | x <- xs, y <- ys, z <- zs ]  -- lorem
         gridNormGradsMags = map (\p@(x, y, z) -> let (gx, gy, gz) = gradEq p; mag = sqrt(gx ^ 2 + gy ^ 2 + gz ^ 2) in (x, y, z, gx / mag, gy / mag, gz/mag, mag)) gridPoints  -- lorem
         gridNormGradsMagsSorted = sortBy (\(_, _, _, _, _, _, mag1) (_, _, _, _, _, _, mag2) -> compare mag1 mag2) gridNormGradsMags  -- lorem
         arrow_dr = (arrowMax - arrowMin) / fromIntegral (length gridPoints)  -- lorem
         arrowLengths = [arrowMin, arrowMin + arrow_dr ..]  -- lorem
         gridNormGradsLens = zipWith (\(x, y, z, ngx, ngy, ngz, _) len -> (x, y, z, ngx, ngy, ngz, len)) gridNormGradsMagsSorted arrowLengths  -- lorem
         rainbowify (x1, y1, z1, ngx, ngy, ngz, len) = rainbowLine (x1, y1, z1) (x2, y2, z2) mcxMax mcyMax mczMax eqxMin eqxMax eqyMin eqyMax eqzMin eqzMax numEachColor -- lorem
            where xLen = ngx * len -- lorem
                  yLen = ngy * len -- lorem
                  zLen = ngz * len -- lorem
                  x2 = x1 + xLen -- lorem
                  y2 = y1 + yLen -- lorem
                  z2 = z1 + zLen -- lorem
                  mczLen = xLen / (eqxMax - eqxMin) * fromIntegral mczMax -- lorem
                  mcxLen = yLen / (eqyMax - eqyMin) * fromIntegral mcxMax -- lorem
                  mcyLen = zLen / (eqzMax - eqzMin) * fromIntegral mcyMax -- lorem
                  mcLen = sqrt(mczLen ^ 2 + mcyLen ^ 2+ mcxLen ^ 2) -- lorem
                  numEachColor = ceiling (mcLen / 8.0) -- lorem
         plotDotify (x, y, z, ngx, ngy, ngz, len) = plotDot (x, y, z) mcxMax mcyMax mczMax eqxMin eqxMax eqyMin eqyMax eqzMin eqzMax bulbSize 11 -- lorem


-- bumpy sphere makes a bumpy sphere of one color with any desired values of: mean radius, relative amplitude, number of wobbles in both phi and theta, number of plot points in both phi and theta, and the color
bumpySphere :: Float  -- ^the mean radius of the bumpy sphere
  -> Float  -- ^the relative amplitude of the bumpy sphere e.g. if this is set to 0.2, then the radius will range from 0.8 to 1.2 times the mean radius
  -> Float  -- ^the m value is the number of wobbles in the phi direction
  -> Float  -- ^the n value is the number of wobbles in the theta direction
  -> Int  -- ^the number of phi values for plot points
  -> Int  -- ^the number of theta values for plot points
  -> Int  -- ^the color of the bumpy sphere
  -> Plist
bumpySphere meanRadius relativeAmplitude mVal nVal numPhis numThetas color = nub $ map (\(x, y, z) -> (round x, round y, round z, color)) xyzs  -- The resultant P list is got by taking all the plot points in (x, y, z) with co-ordinates and floats, rounding those, and tupling them up with the color of the bumpy sphere.
   where pole1 = (0.0, 0.0, meanRadius)  -- This is the point for one of the z-poles, which will always be at the mean radius in the +z direction.
         pole2 = (0.0, 0.0, negate meanRadius)  -- This is the point for one of the z-poles, which will always be at the mean radius in the -z direction.
         numNonPolarPhis = numPhis - 2  -- The number of non-polar phi values is the number of total phi values minus the two for the poles.
         nonPolarPhis = map indexToAngle [1 .. numNonPolarPhis]  -- This maps all the phi indices to phi values.
            where indexToAngle ind = (pi * fromIntegral ind) / (fromIntegral numPhis - 1.0)  -- This returns a phi value for a phi index.
         thetas = map indexToAngle [1 .. numThetas]  -- This maps all the theta indices to theta values.
            where indexToAngle ind = 2.0 * pi * fromIntegral ind / fromIntegral numThetas  -- This returns a theta value for a theta index.
         rhoPhiThetas = [ let rho = meanRadius * (1 + relativeAmplitude * sin (phi * mVal) * sin (theta * nVal)) in (rho, phi, theta) | phi <- nonPolarPhis, theta <- thetas ]  -- This takes the set of non-polar phi values and the set of theta values, and returns a set of (rho, phi, theta) triples by applying the bumpy sphere equation.
         xyzs = concat [[pole1], map rpt_to_xyz rhoPhiThetas, [pole2]]  -- This takes all the (rho, phi, theta) triples for all the plot points and converts them to (x, y, z) by the equations for converting spherical co-ordinates to cartesian, and adds the two pole points.
            where rpt_to_xyz (rho, phi, theta) = (x, y, z)  -- This applies all three components of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                     where x = rho * cos theta * sin phi  -- This is the x component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                           y = rho * sin theta * sin phi  -- This is the y component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                           z = rho * cos phi  -- This is the z component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).


-- bumpy sphere rainbow makes a bumpy sphere of rainbow colors with any desired values of: mean radius, relative amplitude, number of wobbles in both phi and theta, number of plot points in both phi and theta, and the rainbow number.
bumpySphereRainbow :: Float  -- ^the mean radius of the bumpy sphere
  -> Float  -- ^the relative amplitude of the bumpy sphere e.g. if this is set to 0.2, then the radius will range from 0.8 to 1.2 times the mean radius
  -> Float  -- ^the m value is the number of wobbles in the phi direction
  -> Float  -- ^the n value is the number of wobbles in the theta direction
  -> Int  -- ^the number of phi values for plot points
  -> Int  -- ^the number of theta values for plot points
  -> Int  -- ^the rainbow number (1 or 2)
  -> Plist
bumpySphereRainbow meanRadius relativeAmplitude mVal nVal numPhis numThetas rainbNum = nub $ map (\(x, y, z, c) -> (round x, round y, round z, c)) xyzcs  -- The resultant P list is got by taking all the plot points with colors (x, y, z, c) with co-ordinates and floats and rounding those.
   where midColor = if rainbNum == 1 then 6 else 11  -- This is the color at the middle of the rainbow.
         pole1 = (0.0, 0.0, meanRadius, midColor)  -- This is the point for one of the z-poles, which will always be at the mean radius in the +z direction and at a middling color.
         pole2 = (0.0, 0.0, negate meanRadius, midColor)  -- This is the point for one of the z-poles, which will always be at the mean radius in the -z direction and at a middling color.
         numNonPolarPhis = numPhis - 2  -- The number of non-polar phi values is the number of total phi values minus the two for the poles.
         nonPolarPhis = map indexToAngle [1 .. numNonPolarPhis]  -- This maps all the phi indices to phi values.
            where indexToAngle ind = (pi * fromIntegral ind) / (fromIntegral numPhis - 1.0)  -- This returns a phi value for a phi index.
         thetas = map indexToAngle [1 .. numThetas]  -- This maps all the theta indices to theta values.
            where indexToAngle ind = 2.0 * pi * fromIntegral ind / fromIntegral numThetas  -- This returns a theta value for a theta index.
         rhoPhiThetas = [ let rho = meanRadius * (1 + relativeAmplitude * sin (phi * mVal) * sin (theta * nVal)) in (rho, phi, theta) | phi <- nonPolarPhis, theta <- thetas ]  -- This takes the set of non-polar phi values and the set of theta values, and returns a set of (rho, phi, theta) triples by applying the bumpy sphere equation.
         xyzcs = concat [[pole1], map rpt_to_xyzc rhoPhiThetas, [pole2]]  -- This takes all the (rho, phi, theta) triples for all the plot points and converts them to (x, y, z, c), getting the (x, y, z) by the equations for converting spherical co-ordinates to cartesian, getting colors scaled to rho range, and adds the two pole points.
            where rpt_to_xyzc (rho, phi, theta) = (x, y, z, c)  -- This applies all three components of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z), and scales colors to rho range.
                     where x = rho * cos theta * sin phi  -- This is the x component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                           y = rho * sin theta * sin phi  -- This is the y component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                           z = rho * cos phi  -- This is the z component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                           c = min numColors $ max 1 $ {- The part of the line before this is a couple of safety checks for underflowing or overflowing the number of colors in the rainbow -} floor $ (fromIntegral numColors + 0.0001) * relativeRho  -- This is the color number from scaling the relative rho to the rainbow.
                              where minRho = meanRadius * (1 - relativeAmplitude)  -- This is the minimum rho value, which is the local radius at the deepest part of all the inward wave parts, and equal to the mean radius scaled down by the relative amplitude
                                    maxRho = meanRadius * (1 + relativeAmplitude)  -- This is the maximum rho value, which is the local radius at the most prominent part of all the inward wave parts, and equal to the mean radius scaled up by the relative amplitude
                                    rhoRange = maxRho - minRho  -- This is the range of rho values = the maximum minus the minimum.
                                    relativeRho = (rho - minRho) / rhoRange  -- This is a rho value normalized to the range of 0 to 1 where 0 is the minimum possible amplitude and 1 is the maximum possible amplitude.
                                    numColors = case rainbNum of  -- Rainbow number 1 has exactly 11 colors and rainbow number 2 has exactly 22 colors.
                                                   1 -> 11
                                                   2 -> 22
                                                   _ -> undefined


-- sdfractal is a fractal whose name I don't know (if it has one), so I call it "the sneezing demon".  It has an octahedral envelope, but isn't a Sierpinski octahedron.  Give this function a level number and it gives you all the blocks for that level of the fractal pattern.  
sdFractal :: Int -- ^ the level number
  -> Int -- ^ the color number
  -> Plist
sdFractal 1 color = [(0, 0, 0, color)]  -- Level 1 of the pattern is 1 block.
sdFractal levelNum color = concat [upPart, downPart, northPart, eastPart, southPart, westPart, nextSmallerSdfractal]  -- Any given level of the pattern is based on the next smaller level.  A level other than the base level is got by copying the next smaller level in place, then copying halves of that next smaller level in each of the cardinal directions.
   where nextSmallerSdfractal = sdFractal (pred levelNum) color  -- The next smaller level of the pattern is got by calling the same function with the next smaller level number.
         upPart = let halfPart = filter (\(x, y, z, c) -> y >= 0) nextSmallerSdfractal in movePListUp halfPart (2 ^ (levelNum - 2))  -- This takes the next smaller level and copies the upper half upward.
         downPart = let halfPart = filter (\(x, y, z, c) -> y <= 0) nextSmallerSdfractal in movePListDown halfPart (2 ^ (levelNum - 2))  -- This takes the next smaller level and copies the lower half downward.
         northPart = let halfPart = filter (\(x, y, z, c) -> z <= 0) nextSmallerSdfractal in movePListNorth halfPart (2 ^ (levelNum - 2))  -- This takes the next smaller level and copies the northern half northward.
         eastPart = let halfPart = filter (\(x, y, z, c) -> x >= 0) nextSmallerSdfractal in movePListEast halfPart (2 ^ (levelNum - 2))  -- This takes the next smaller level and copies the eastern half eastward.
         southPart = let halfPart = filter (\(x, y, z, c) -> z >= 0) nextSmallerSdfractal in movePListSouth halfPart (2 ^ (levelNum - 2))  -- This takes the next smaller level and copies the southern half southward.
         westPart = let halfPart = filter (\(x, y, z, c) -> x <= 0) nextSmallerSdfractal in movePListWest halfPart (2 ^ (levelNum - 2))  -- This takes the next smaller level and copies the western half westward.


-- sierpoct is a voxellized Sierpinski octahedron, which is an octahedral 3D fractal.  This is the magic golfed version of the code.
sierpOct :: Int -- ^ the number for which level of the pattern to get
  -> Int -- ^ the color number
  -> Plist
sierpOct levelNum color = map (\(x, y, z) -> (x, y, z, color)) (sierpOctLocs levelNum)  -- The result is from zipping the block locations with the color number.
   where sierpOctLocs 1 = [(0, 0, 0)]  -- Level 1 of the pattern is just one block.
         sierpOctLocs levelNum = (foldr1(.)$take (pred levelNum)$zipWith($)(repeat(\e a->a>>=(\d(a,b,c)->[(a+d,b,c),(a-d,b,c),(a,b+d,c),(a,b-d,c),(a,b,c+d),(a,b,c-d)])(2^e)))[0..])$[(0,0,0)]  -- This line of the code is golfed magic.  It returns the locations of the blocks.


--   -------- Subsection of plotting functions: getting to U lists --------
--   ----------------------------------------------------------------------

-- scalar field 2D to U list takes a 2D array of scalar values and turns them into a list of U sets representing a plot filling a level plane in Minecraft within certain x and z bounds at some height and the U value for each is just the scalar value unchanged.
sf2DtoUList :: ScalarField2D -> Ulist
sf2DtoUList inpSF = [(zi, 100, xi, uVal zi xi) | zi <- zVals, xi <- xVals]  -- For each x value and for each z value, the U set has those x and z values, height of 100, and U value corresponding to using that x and z to index a row and column of the input.
   where dim2 = length inpSF  -- The second dimension size is the number of points in the second dimension of the 2D array.
         dim1 = length $ head inpSF  -- The first dimension size is the number of points in the first dimension of the 2D array.  This assumes all dim1s (of all dim2-rows) will be equal to the size of the first.
         xVals = [1 .. dim2]  -- The list of x values ranges from 1 to the second dimension size.
         zVals = [1 .. dim1]  -- The list of z values ranges from 1 to the first dimension size.
         uVal zi xi = (inpSF !! pred zi) !! pred xi  -- The U value for the result is ripped straight from the input.


-- scalar field 3D to U list takes a 3D array of scalar values and turns them into a list of U sets representing a 3D plot of points, a 3D grid of dots with spaces between, where each dot is a minecraft block or clump of blocks with color representing field magnitude at each location.  The U value for each is just the scalar value unchanged.
sf3DtoUList :: ScalarField3D  -- ^A 3D array of scalar values
  -> Int  -- ^clump size is how many blocks to a clump e.g. set it to 1 for each plot point to be one block, set to 2 for each plot to be a clump of 8 blocks (2 by 2 by 2)
  -> Int  -- ^space size is how many blocks are between successive clumps
  -> Ulist
sf3DtoUList inpSF clumpSize spaceSize = [(mc_x eq_y_ind dy, mc_y eq_z_ind dz, mc_z eq_x_ind dx, uVal eq_x_ind eq_y_ind eq_z_ind) | eq_x_ind <- eq_x_inds, eq_y_ind <- eq_y_inds, eq_z_ind <- eq_z_inds, dx <- dxs, dy <- dys, dz <- dzs]  -- The resultant U list takes care of every index in x, y, and z, and every dx, dy, dz with a block for each.  There is a conversion between Minecraft (x, y, z) and equation (x, y, z).
   where pitch = clumpSize + spaceSize  -- The pitch of the pattern is the sum of how big a block is and how far the space between clumps is.
         dim3 = length inpSF  -- The third dimension size is the number of points in the third dimension of the 3D array.
         dim2 = length $  head inpSF  -- The second dimension size is the number of points in the second dimension of the 3D array.  This assumes all dim2s (of all dim3-rows) will be equal to the size of the first.
         dim1 = length $ (head . head) inpSF  -- The first dimension size is the number of points in the first dimension of the 3D array.  This assumes all dim1s (of all dim2-rows of all dim3-rows) will be equal to the size of the first of the first.
         eq_z_inds = [1 .. dim3]  -- The list of z indices ranges from 1 to the third dimension size.
         eq_y_inds = [1 .. dim2]  -- The list of y indices ranges from 1 to the second dimension size.
         eq_x_inds = [1 .. dim1]  -- The list of x indices ranges from 1 to the first dimension size.
         [dxs, dys, dzs] = replicate 3 [1 .. clumpSize]  -- dxs is a list of offsets for the blocks within a clump in the x direction, similar dys and y direction, dzs and z direction.
         mc_x eq_y_ind dy = pred eq_y_ind * pitch + floor (0.5 * fromIntegral pitch) + dy  -- The Minecraft x location of a block is based on the product of the equation y index multiplied by the plot's pitch, with an adjustment for alignment, plus a dy to locate the block within the clump.
         mc_y eq_z_ind dz = pred eq_z_ind * pitch + floor (0.5 * fromIntegral pitch) + dz  -- The Minecraft y location of a block is based on the product of the equation z index multiplied by the plot's pitch, with an adjustment for alignment, plus a dz to locate the block within the clump.
         mc_z eq_x_ind dx = pred eq_x_ind * pitch + floor (0.5 * fromIntegral pitch) + dx  -- The Minecraft z location of a block is based on the product of the equation x index multiplied by the plot's pitch, with an adjustment for alignment, plus a dx to locate the block within the clump.
         uVal eq_x_ind eq_y_ind eq_z_ind = ((inpSF !! pred eq_z_ind) !! pred eq_y_ind) !! pred eq_x_ind  -- The U value for the result is ripped straight from the input.


-- scalar field 3D to U list takes a 3D array of scalar values and turns them into a list of U sets representing a 3D plot of points, a 3D grid of dots with spaces between, where each dot is a minecraft block or clump of blocks with color representing field magnitude at each location.  The U value for each is just the scalar value unchanged.
sf3DSlicedtoUList :: ScalarField3DSliced  -- ^a sliced 3D scalar field
  -> Int  -- ^slice direction
  -> Int  -- ^pitch
  -> Ulist
sf3DSlicedtoUList inpSFS sliceDirection pitch = [(mc_x eq_y_ind, mc_y eq_z_ind, mc_z eq_x_ind, uVal eq_x_ind eq_y_ind eq_z_ind) | eq_x_ind <- eq_x_inds, eq_y_ind <- eq_y_inds, eq_z_ind <- eq_z_inds]  -- The resultant U list takes care of every index in x, y, and z.  There is a conversion between Minecraft (x, y, z) and equation (x, y, z).
   where uVal xi yi zi = ((inpSFS !! pred zi) !! pred yi) !! pred xi  -- The U value for the result is ripped straight from the input.
         dim3 = length inpSFS  -- The third dimension size is the number of points in the third dimension of the 3D array.
         dim2 = length $  head inpSFS  -- The second dimension size is the number of points in the second dimension of the 3D array.  This assumes all dim2s (of all dim3-rows) will be equal to the size of the first.
         dim1 = length $ (head . head) inpSFS  -- The first dimension size is the number of points in the first dimension of the 3D array.  This assumes all dim1s (of all dim2-rows of all dim3-rows) will be equal to the size of the first of the first.
         eq_z_inds = [1 .. dim3]  -- The list of z indices ranges from 1 to the third dimension size.
         eq_y_inds = [1 .. dim2]  -- The list of y indices ranges from 1 to the second dimension size.
         eq_x_inds = [1 .. dim1]  -- The list of x indices ranges from 1 to the first dimension size.
         mc_x eq_y_ind = case sliceDirection of  -- The distance between successive Minecraft x points depends on the slice direction.  If slice direction 2 (equation y), then pitch is the number of spaces between, otherwise no space between.
                                1 -> eq_y_ind
                                2 -> pred eq_y_ind * pitch
                                3 -> eq_y_ind
                                _ -> undefined
         mc_y eq_z_ind = case sliceDirection of  -- The distance between successive Minecraft y points depends on the slice direction.  If slice direction 3 (equation z), then pitch is the number of spaces between, otherwise no space between.
                                1 -> eq_z_ind
                                2 -> eq_z_ind
                                3 -> pred eq_z_ind * pitch
                                _ -> undefined
         mc_z eq_x_ind = case sliceDirection of  -- The distance between successive Minecraft z points depends on the slice direction.  If slice direction 1 (equation x), then pitch is the number of spaces between, otherwise no space between.
                                1 -> pred eq_x_ind * pitch
                                2 -> eq_x_ind
                                3 -> eq_x_ind
                                _ -> undefined


--   -------- Subsection of plotting functions: getting from U lists to P lists --------
--   -----------------------------------------------------------------------------------

-- scale U list linear converts a U list to a P list, which means to turn magnitudes into colors.  Linear means each block color represents the same range size of magnitude values (doesn't try for equal numbers of block colors).  You can specify some number of the lowest and highest values to be given coldest and hottest colors (respectively) and be excluded from the magnitude calculations.
scaleUListLinear :: Ulist  -- ^the input U list
  -> Int  -- ^the rainbow number (1 or 2)
  -> Int  -- ^the number of low values to assign the min color and exclude from scaling calculations
  -> Int  -- ^the number of high values to assign the max color and exclude from scaling calculations
  -> Plist
scaleUListLinear inpList rainbNum numLowDrops numHighDrops = concat [lowPList, midPList, highPList]  -- The resultant list has the low values at color 1, the middling values with colors ranging from 1 to max, and the high values with color max.
   where sortedInpList = sortBy (\(_, _, _, u1) (_, _, _, u2) -> compare u1 u2) inpList  -- This takes the input U list and sorts the entries in terms of low to high magnitudes.
         midInpList = drop numLowDrops $ reverse $ drop numHighDrops $ reverse sortedInpList  -- This takes the sorted input list and takes some number of entries of the high and low ends, based on the values from the input.
         midMinMag = minimum $ map (\(_, _, _, m) -> m) midInpList  -- This returns the smallest value from the list that's been sorted and trimmed.
         midMaxMag = maximum $ map (\(_, _, _, m) -> m) midInpList  -- This returns the biggest value from the list that's been sorted and trimmed.
         midMagRange = midMaxMag - midMinMag  -- This is the difference between the biggest and smallest values from the list that's been sorted and trimmed.
         lowInps = take numLowDrops sortedInpList  -- This is the list of low inputs that were trimmed of the original list.
         highInps = reverse $ take numHighDrops $ reverse sortedInpList  -- This is the list of high inputs that were trimmed of the original list.
         lowPList = map (\(x, y, z, u) -> (x, y, z, 1)) lowInps  -- This takes the low trimmed inputs and gives them all color 1.
         highPList = map (\(x, y, z, u) -> (x, y, z, numColors)) highInps  -- This takes the high trimmed inputs and gives them all the highest color number.
         numColors = case rainbNum of  -- Rainbow number 1 has exactly 11 colors and rainbow number 2 has exactly 22 colors.
                        1 -> 11
                        2 -> 22
                        _ -> undefined
         midPList = map uSetToPSet midInpList  -- This maps the function for converting a U value to a color to the trimmed sorted input list.
            where uSetToPSet (x, y, z, uVal) = (x, y, z, colorUVal uVal)  -- Changing a U set to a P set is to apply a colorizing function to the U value and leave the co-ordinates the same.
                  colorUVal uVal = min numColors $ max 1 $ {- The part of the line before this is a couple of safety checks for underflowing or overflowing the number of colors in the rainbow -} floor $ (fromIntegral numColors + 0.0001) * relativeUVal uVal  -- This assigns a relative U value (between 0 and 1) to a position along the rainbow.
                  relativeUVal uVal = (uVal - midMinMag) / midMagRange  -- This returns a number that should be normalized to the range of 0 to 1 and represents a given u value's relative size within the trimmed sorted input list.


-- scale U list equal colors converts a U list to a P list, which means to turn magnitudes into colors.  This type of scaling assigns equals numbers of each block color (doesn't try for equal range sizes for each color).
scaleUListEqualColors :: Ulist  -- ^the input U list
  -> Int  -- ^the rainbow number (1 or 2)
  -> Plist
scaleUListEqualColors inpList rainbNum = zipWith (\(x, y, z, _) c -> (x, y, z, c)) sortedInpList colorList  -- The result zips the co-ordinate list sorted by magnitude with the sorted color list.
   where sortedInpList = sortBy (\(_, _, _, u1) (_, _, _, u2) -> compare u1 u2) inpList  -- This takes the input U list and sorts the entries in terms of low to high magnitudes.
         numEachColor = fromIntegral (length inpList) / numColors  -- The number of blocks of each color is equal to the number of blocks divided by the number of colors.
         numColors = case rainbNum of  -- Rainbow number 1 has exactly 11 colors and rainbow number 2 has exactly 22 colors.
                            1 -> 11.0
                            2 -> 22.0
                            _ -> undefined
         changeInds = map ceiling [numEachColor, 2 * numEachColor ..]  -- This returns a list of indices where the color changes should occur and takes care of the rounding to whole numbers.
         numPerColor = map (\a -> (changeInds !! a) - (changeInds !! pred a)) [1 ..]  -- This returns a list of how many blocks of each color there are.  e.g. if there were 9 colors and 21 blocks, the list would be something like [2, 2, 3, 2, 2, 3, 2, 2, 3], to disperse which numbers are rounded up and which ones are rounded down.
         colorNumSubList colorNum = replicate (numPerColor !! pred colorNum) colorNum  -- This returns a list with a color repeated a number of times equal to the number of blocks that should be that color.
         colorList = concatMap colorNumSubList [1 ..]  -- This returns a list with each color repeated the number of times equal to the number of blocks that should be that color.  e.g. if there were 9 colors and 21 blocks, the list would be something like [1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6, 7, 7, 8, 8, 9, 9, 9].


-- scale U list linear with height converts a U list to a P list, which means to turn magnitudes into colors.  Linear means each block color represents the same range size of magnitude values (doesn't try for equal numbers of block colors).  You can specify some number of the lowest and highest values to be given coldest and hottest colors (respectively) and be excluded from the magnitude calculations.  This also adds a height dimension with height steps also scaled linearly to magnitude values.
scaleUListLinearWHeight :: Ulist  -- ^the input U list
  -> Int  -- ^the rainbow number (1 or 2)
  -> Int  -- ^the number of low values to assign the min color and exclude from scaling calculations
  -> Int  -- ^the number of high values to assign the max color and exclude from scaling calculations
  -> Int  -- ^the max height gain to input y values
  -> Int  -- ^thickness = blocks vertically under to fill in spaces where steep
  -> Bool  -- ^True if high magnitude is higher in Minecraft space, False for low magnitudes higher in Minecraft space
  -> Plist
scaleUListLinearWHeight inpList rainbNum numLowDrops numHighDrops maxHeight thickness highIsHigh = concatMap (movePListDown unthickenedResultPList) [1 .. thickness]  -- This get the result by copying the unthickened graph down to each distance from 1 to thickness value.
   where sortedInpList = sortBy (\(_, _, _, u1) (_, _, _, u2) -> compare u1 u2) inpList  -- This takes the input U list and sorts the entries in terms of low to high magnitudes.
         midInpList = drop numLowDrops $ reverse $ drop numHighDrops $ reverse sortedInpList  -- This takes the sorted input list and takes some number of entries of the high and low ends, based on the values from the input.
         midMinMag = minimum $ map (\(_, _, _, m) -> m) midInpList  -- This returns the smallest value from the list that's been sorted and trimmed.
         midMaxMag = maximum $ map (\(_, _, _, m) -> m) midInpList  -- This returns the biggest value from the list that's been sorted and trimmed.
         midMagRange = midMaxMag - midMinMag  -- This is the difference between the biggest and smallest values from the list that's been sorted and trimmed.
         lowInps = take numLowDrops sortedInpList  -- This is the list of low inputs that were trimmed of the original list.
         highInps = reverse $ take numHighDrops $ reverse sortedInpList  -- This is the list of high inputs that were trimmed of the original list.
         lowPList = map (\(x, y, z, u) -> (x, if highIsHigh then y else y + maxHeight, z, 1)) lowInps  -- The p List for the low magnitude values dropped out of the scaling equations assigns minimum color and height, unless high and low are reversed, in which case minimum color and maximum height.
         highPList = map (\(x, y, z, u) -> (x, if highIsHigh then y + maxHeight else y, z, numColors)) highInps  -- The p List for the high magnitude values dropped out of the scaling equations assigns maximum color and height, unless high and low are reversed, in which case maximum color and minimum height.
         numColors = case rainbNum of  -- Rainbow number 1 has exactly 11 colors and rainbow number 2 has exactly 22 colors.
                        1 -> 11
                        2 -> 22
                        _ -> undefined
         midPList = map uSetToPSet midInpList  -- This maps the function for converting U sets to P sets to the trimmed sorted input list.
            where uSetToPSet (x, inp_y, z, uVal) = (x, new_y, z, colorUVal uVal)  -- The function that converts a U set to a P set assigs a color and a height gain to the magnitude value.
                     where colorUVal uVal = min numColors $ max 1 $ {- The part of the line before this is a couple of safety checks for underflowing or overflowing the number of colors in the rainbow -} floor $ (fromIntegral numColors + 0.0001) * relativeUVal  -- This assigns a relative U value (between 0 and 1) to a position along the rainbow.
                           relativeUVal = (uVal - midMinMag) / midMagRange  -- This returns a number that should be normalized to the range of 0 to 1 and represents a given u value's relative size within the trimmed sorted input list.
                           heightGain = let unflippedHeightGain = floor $ relativeUVal * fromIntegral maxHeight in if highIsHigh then unflippedHeightGain else maxHeight - unflippedHeightGain  -- This assigns an amount for the height gain by multuplying a normalized value with the maximum height gain and possibly flips it based on the input setting.
                           new_y = inp_y + heightGain  -- The resultant P set's y value is the input U set's y value plus the height gain.
         unthickenedResultPList = concat [lowPList, midPList, highPList]  -- The P list for the graph before thickening is got by joining the P lists for the values cut out of the range calculation and the ones that weren't.


-- scale U list equal colors with height converts a U list to a P list, which means to turn magnitudes into colors.  This type of scaling assigns equals numbers of each block color (doesn't try for equal range sizes for each color) and also adds a height dimension with equal numbers of blocks at each height.
scaleUListEqualColorsWHeight :: Ulist  -- ^the input U list
  -> Int  -- ^the rainbow number (1 or 2)
  -> Int  -- ^the max height gain to input y values
  -> Int  -- ^thickness = blocks vertically under to fill in spaces where steep
  -> Bool  -- ^True if high magnitude is higher in Minecraft space, False for low magnitudes higher in Minecraft space
  -> Plist
scaleUListEqualColorsWHeight inpList rainbNum maxHeight thickness highIsHigh = concatMap (movePListDown unthickenedResultPList) [1 .. thickness]  -- This get the result by copying the unthickened graph down to each distance from 1 to thickness value.
   where sortedInpList = sortBy (\(_, _, _, u1) (_, _, _, u2) -> compare u1 u2) inpList  -- This takes the input U list and sorts the entries in terms of low to high magnitudes.
         numEachColor = fromIntegral (length inpList) / numColors  -- The number of blocks of each color is equal to the number of blocks divided by the number of colors.
         numColors = case rainbNum of  -- Rainbow number 1 has exactly 11 colors and rainbow number 2 has exactly 22 colors.
                            1 -> 11.0
                            2 -> 22.0
                            _ -> undefined
         colorChangeInds = map ceiling [numEachColor, 2 * numEachColor ..]  -- This returns a list of indices where the color changes should occur and takes care of the rounding to whole numbers.
         numPerColor = map (\a -> (colorChangeInds !! a) - (colorChangeInds !! pred a)) [1 ..]  -- This returns a list of how many blocks of each color there are.  e.g. if there were 9 colors and 21 blocks, the list would be something like [2, 2, 3, 2, 2, 3, 2, 2, 3], to disperse which numbers are rounded up and which ones are rounded down.
         colorNumSubList colorNum = replicate (numPerColor !! pred colorNum) colorNum  -- This returns a list with a color repeated a number of times equal to the number of blocks that should be that color.
         colorList = concatMap colorNumSubList [1 ..]  -- This returns a list with each color repeated the number of times equal to the number of blocks that should be that color.  e.g. if there were 9 colors and 21 blocks, the list would be something like [1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6, 7, 7, 8, 8, 9, 9, 9].
         numEachHeight = fromIntegral (length inpList) / fromIntegral maxHeight  -- The number of blocks at each height is equal to the number of blocks divided by the number of height levels.
         heightChangeInds = map ceiling [numEachHeight, 2 * numEachHeight ..]  -- This returns a list of indices where the height changes should occur and takes care of the rounding to whole numbers.
         numPerHeight = map (\a -> (heightChangeInds !! a) - (heightChangeInds !! pred a)) [1 ..]  -- This returns a list of how many blocks of each height there are.  See note on numPerColor: this is like that but with the heights rather than the colors.
         heightNumSubList heightNum = replicate (numPerHeight !! pred heightNum) heightNum  -- This returns a list with a height repeated a number of times equal to the number of blocks that should be that height.
         heightList = let unflippedHeightList = concatMap heightNumSubList [1 ..] in if highIsHigh then unflippedHeightList else map (maxHeight -) unflippedHeightList  -- This returns a list with each height repeated the number of times equal to the number of blocks that should be that height. See note on colorList:  this is like that but with the heights rather than the colors.
         unthickenedResultPList = zipWith3 (\(x, y, z, _) c dy -> (x, y + dy, z, c)) sortedInpList colorList heightList -- The P list for the graph (before thickening) is got by zipping together the list of (x, y, z) locations sorted by value, the color list, and the height list by adding the heights to the locations and tupling up the colors with those.


-- mono color U list converts a U list to a P list, which means to turn magnitudes into colors.  This one is used when you don't want to scale magnitude values to colors and just want to color all the blocks the same color.
monoColorUList :: Ulist -> Int -> Plist
monoColorUList inpList color = map (\(x, y, z, _) -> (x, y, z, color)) inpList


--                /------------------------------------------------------\
--                |------------------------------------------------------|
--                |---------------  - Output Functions -  ---------------|
--                |------------------------------------------------------|
--                \------------------------------------------------------/

-- py string a block takes a P set and a rainbow number and returns the two lines of MCEdit python for plotting a block of some type at some location.  It might be colored wool (with a color data value) or some other block type (without an additional data value).
pyStringABlock :: Pset -> Int -> String
pyStringABlock (inp_x, inp_y, inp_z, inpC) rainbNum = case rainbNum of  -- This calls the function for turning a block's P set into MCEdit python, whatever the rainbow number.
                                                      1 -> rainb1Str
                                                      2 -> rainb2Str
                                                      4 -> rainb4Str  -- rainb4str other blocks for sculptures and stuff
                                                      _ -> undefined
   where coordStr = show inp_x ++ ", " ++ show inp_y ++ ", " ++ show inp_z  -- This takes the co-ordinates from the input P set and stringifies them, comma-delimited.
         setBlockStr blockNum = concat ["    level.setBlockAt(", coordStr, ", ", show blockNum, ")\n"]  -- This is the MCEdit python line for setting a block at some location with some number for the type of Minecraft block (e.g. is it a wool block or a diamond block).
         setDataStr dataNum = concat ["    level.setBlockDataAt(", coordStr, ", ", show dataNum, ")\n"]  -- This is the MCEdit python line for setting the data value of some block - not the same as the block number.  This line is for setting the color of a wool block when the block type is wool.
         rainb1Str = setBlockStr 35 ++ setDataStr (woolRainbowLookup !! pred inpC)  -- When the input rainbow number is 1, all blocks are wool blocks.
            where woolRainbowLookup = [10, 2, 3, 9, 11, 13, 5, 4, 1, 6, 14, 0, 8, 7, 15]  -- This is the list of wool block data values in rainbow order.
         rainb2Str = if inpC `elem` woolInds then woolStr inpC else nonWoolStr inpC  -- When the input rainbow number is 2, then some blocks are wool blocks (requiring a line for setting the color) and some blocks are not wool blocks (and don't require a line for setting the color).  This line checks whether a color number (between 1 and 22) is the index of a wool block or not, and then calls the thing that gets the string in either case.
            where woolStr cNum = setBlockStr 35 ++ setDataStr (woolDatas !! pred cNum)  -- This returns the MCEdit python line for setting a wool block and the data value for that block's color.
                  woolInds = [1, 2, 3, 6, 7, 9, 11, 13, 17, 18, 19]  -- This is the list of 22-color rainbow indices which are wool blocks.
                  woolDatas = [10, 2, 3, 0, 0, 9, 11, 0, 13, 0, 5, 0, 4, 0, 0, 0, 1, 6, 14, 0, 0, 0]  -- This is the list of wool block data values in rainbow order plus zeros where non-wool blocks are in the 22-color rainbow.
                  nonWoolStr cNum = setBlockStr $ blockIDs !! pred cNum  -- This returns the MCDdit python line for setting a non-wool block (with no second line for setting block data value).
                  blockIDs = [35, 35, 35, 79, 57, 35, 35, 21, 35, 48, 35, 41, 35, 19, 172, 86, 35, 35, 35, 73, 87, 45]  -- This is the list of block ID numbers for different block types e.g. one is for diamond, another for emerald, and all the 35s are wool.
         rainb4Str = if inpC `elem` withDataInds then withDataStr inpC else noDataStr inpC  -- 
            where withDataStr cNum = setBlockStr (blockIDs !! pred cNum) ++ setDataStr (blockDatas !! pred cNum)  -- 
                  withDataInds = [2, 6, 7]  -- 
                  blockDatas = [0, 1, 0, 0, 0, 3, 2, 0]  -- 
                  noDataStr cNum = setBlockStr $ blockIDs !! pred cNum  -- 
                  blockIDs = [152, 44, 17, 57, 22, 35, 155, 20]  -- 


-- py string a plot takes a whole plot and turns it into a string.  For any graph that isn't tiny, this will generate a long .py file.  It will have a line at the top and then a line or two for each block.
pyStringAPlot :: Plist -> Int -> String
pyStringAPlot inpPList rainbNum = "def perform(level, box, options):\n" ++ concatMap (\x -> pyStringABlock x rainbNum) inpPList  -- The result starts with a header line and then has all the blocks stringified.


-- py strings a plot takes a whole plot and turns it into either one or more strings, each resultant string being the contents of an output py.  If the plot is small, the result of this will be one string for one output py.  If the plot is big, the result will be some bigger number of strings, one string for one output py each.  The maximum for each output py is to take care of 20000 plot points, so if you give this a plot of 50123 blocks, there will be a list of 3 outputs.
pyStringsAPlot :: Plist  -- ^the P list for some plot
  -> Int  -- ^the rainbow number (either 1 or 2)
  -> Bool  -- ^a bool for whether to crop blocks above the Minecraft ceiling
  -> [String]
pyStringsAPlot inpPList rainbNum cropHighYN = if length mbcInpPlist < 20000 then [pyStringAPlot mbcInpPlist rainbNum] else map oneOpString [1,2..numPys]  -- If the maybe cropped input list is short, then the output can be one py file.  If it's long, then it will be more than 1 py file, and let's call each of those a subplot.
   where mbcInpPlist = if cropHighYN then filter (\(_, y, _, _) -> y < 256) inpPList else inpPList  -- The maybe cropped input list is like the input list but with high blocks removed if the input Bool is True, otherwise identical.
         numPys = ceiling $ (fromIntegral . length) mbcInpPlist / 20000.0  -- This is the number of py files that will be output, with a maximum of 20,000 blocks per each output py.
         startInds = 1 : map (\x -> succ (20000 * x)) [1, 2 .. pred numPys]  -- This is a list, [1, 20001, 40001, ..., (ending at some number)] of indices for each output py file, where each is the index of the plot point that shall start each py. 
         endInds = map (\x -> (if x < numPys then 20000 * x else length mbcInpPlist)) [1, 2 .. numPys]  -- This is a list of indices for each output py file, where each is the index of the plot point that shall end each py.  e.g. if there are 50123 plot points, this list will be [20000, 40000, 50123].
         oneOpString outputNum = pyStringAPlot pListSubset rainbNum  -- This generates the output py file for one of the subplots.
            where startInd = startInds !! pred outputNum  -- This selects the start index for this output py from the list of start indices.
                  endInd = endInds !! pred outputNum  -- This selects the end index for this output py from the list of end indices.
                  inds = [startInd, succ startInd .. endInd]  -- This is the full list of indices for a given output py.  e.g. if there are 50123 blocks, the second list of indices will be all the whole numbers from 20001 to 40000.
                  pListSubset = map ((mbcInpPlist !!) . pred) inds  -- This is a subset of the maybe cropped input list corresponding to the blocks of some given plot.


-- js string a plot is for when you want to output to a web browser for a quick render.  Generating a graph this way is much quicker than making a Minecraft world and adding blocks through MCEdit, but this way has not nearly as much picture quality or rendering power.  Use this when making quick adjustments to graphs before final render to Minecraft.  Running this will output a .js file.  Have a .html file load it as a script and also load p5.js as a script.  Open that .html file in a web browser and you should have the plot and be able to turn the camera by clicking and dragging.
jsStringAPlot :: Plist -> Int -> String
jsStringAPlot inpPList rainbNum = concat ["let plot = [", plotString, "];\n\n", p5jsFooter]  -- The resultant string to write as a JavaScript program is the line with all the block locations and colors followed by the rest of a short JavaScript program for calling p5.js to render those as cubes.
   where numColors = case rainbNum of  -- Rainbow number 1 has exactly 11 colors and rainbow number 2 has exactly 22 colors.  Rainbow number 3 is used when numbers span 1 to 15 representing a Minecraft plot with 11 rainbow wool colors and 4 black/white/grey ones.
                            1 -> 11
                            2 -> 22
                            3 -> 15
                            _ -> undefined
         p5jsFooter = "function setup() {\n  createCanvas(640, 360, WEBGL);\n}\nfunction draw() {\n background(20,20,43);\n orbitControl();\n normalMaterial();\n translate(0, 0, -100);\n for (let ind = 0; ind < plot.length; ind++) {\n   push();\n   oneBlock = plot[ind];\n   let x = oneBlock[0];\n   let y = oneBlock[1];\n   let z = oneBlock[2];\n   let r = oneBlock[3];\n   let g = oneBlock[4];\n   let b = oneBlock[5];\n   translate(x, y, z);\n   fill(r, g, b);\n   box(1, 1, 1);\n   pop();\n }\n}"  -- This is most of the javascript output.  When the plot line is added to this and it's output as a .js, it should be a JavaScript program that calls on p5.js to render cubes in a 3D space.
         blockStrings = map showBlock inpPList  -- The list of strings for all blocks is got by mapping a function for showing one block to the list of all blocks.
            where showBlock (x, y, z, colorNumber) = "[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show r ++ "," ++ show g ++ "," ++ show b ++ "]"  -- This displays the block's location (x, y, z) and color (r, g, b) as a list of 6 values in JavaScript.
                     where sliceSize = 360.0 / fromIntegral numColors  -- The set of all strings for all the blocks will be got by mapping something to the input P list.  A circle is sliced into numColors slices and each slice is sliceSize degrees wide.
                           blockHue = fromIntegral (pred colorNumber) * sliceSize  -- The hue for one block is the slize size multiplied by the color number.
                           (r, g, b) = rgbMaxSat (360.0 - blockHue)  -- This returns the block's RGB values which are for a color at maximum saturation and brightness for the block's hue.
         plotString = intercalate "," blockStrings  -- The string for the plot line in the output is got by joining all the strings for each block, separated by more commas to notate a list of lists in JavaScript.


-- write ops take some name, and some list of strings, each string for an output py, and generates a list of IO operations where each operation is to write a py file with a sequential name made of the input name plus a sequential number.  e.g. if given "sdFractal" and a list of 3 strings, it will write foo1.py consisting of the first string, foo2.py consisting of the second, foo3.py consisting of the third.
writeOps :: String -- ^ the name of the series for the sequential filenames
  -> [String] -- ^ the list of output strings for the py files
  -> [IO ()]
writeOps seriesName pyStrList = map oneOp inds  -- The result is got by mapping the function that calls writeFile with the list of output indices.
   where inds = [1,2..length pyStrList]  -- The list of output indices ranges from 1 to the number of strings from the input.
         fileNames = map (\x -> seriesName ++ show x ++ ".py") inds  -- The file names are sequential with the input name, a number, and extension .py
         oneOp ind = writeFile (fileNames !! pred ind) (pyStrList !! pred ind)  -- One output operation matches one sequential name with one string and writes one file.




--                /------------------------------------------------------\
--                |------------------------------------------------------|
--                |-------------  - Results and Outputs -  --------------|
--                |------------------------------------------------------|
--                \------------------------------------------------------/

--   -------- Subsection of results and outputs: scalar field and gradient equations --------
--   ----------------------------------------------------------------------------------------

sfEqa2Df1, sfEqa2Df2, sfEqa2Df3, sfEqa2Df4, sfEqa2Df5, sfEqa2Df6 :: Float -> Float -> Float
-- 2D scalar field equation 1 is for a monopole at the origin that gives off positive values (bigger closer).
sfEqa2Df1 x y = 1 / sqrt (x ^ 2 + y ^ 2)

-- 2D scalar field equation 2 is for a monopole at the origin that gives off negative values (bigger negative closer).
sfEqa2Df2 x y = negate $ sfEqa2Df1 x y

-- 2D scalar field equation 3 is for a monopole at (3, 0) that gives off positive values (bigger closer).
sfEqa2Df3 x y = 1 / sqrt ((x - 3) ^ 2 + y ^ 2)

-- 2D scalar field equation 4 is for a monopole at (-3, 0) that gives off positive values (bigger closer).
sfEqa2Df4 x y = 1 / sqrt ((x + 3) ^ 2 + y ^ 2)

-- 2D scalar field equation 5 is for a pair of positive poles, one at (3, 0, 0), and one at (-3, 0, 0).
sfEqa2Df5 x y = sfEqa2Df3 x y + sfEqa2Df4 x y

-- 2D scalar field equation 6 is for a dipole with a positive pole at (3, 0, 0), and a negative pole at (-3, 0, 0).
sfEqa2Df6 x y = sfEqa2Df3 x y - sfEqa2Df4 x y

-- lorem
sfEqa2Df7 x y = x ^ 2 - y

sfEqa3Df1, sfEqa3Df2, sfEqa3Df3, sfEqa3Df4, sfEqa3Df5, sfEqa3Df6, sfEqa3Df7, sfEqa3Df8, sfEqa3Df9 :: Float -> Float -> Float -> Float
-- 3D scalar field equation 1 is for a monopole at the origin that gives off positive values (bigger closer).
sfEqa3Df1 x y z = 1 / sqrt (x ^ 2 + y ^ 2 + z ^ 2)

-- 3D scalar field equation 2 is for a monopole at the origin that gives off negative values (bigger negative closer).
sfEqa3Df2 x y z = negate $ sfEqa3Df1 x y z

-- 3D scalar field equation 3 is for a monopole at (3, 0, 0) that gives off positive values (bigger closer).
sfEqa3Df3 x y z = 1 / sqrt ((x - 3) ^ 2 + y ^ 2 + z ^ 2)

-- 3D scalar field equation 4 is for a monopole at (-3, 0, 0) that gives off positive values (bigger closer).
sfEqa3Df4 x y z = 1 / sqrt ((x + 3) ^ 2 + y ^ 2 + z ^ 2)

-- 3D scalar field equation 6 is for a dipole with two positive poles, at locations (3, 0, 0) and (-3, 0, 0).
sfEqa3Df5 x y z = sfEqa3Df3 x y z + sfEqa3Df4 x y z

-- 3D scalar field equation 6 is for a dipole with a positive pole at (3, 0, 0) and a negative pole at (-3, 0, 0).
sfEqa3Df6 x y z = sfEqa3Df3 x y z - sfEqa3Df4 x y z

-- lorem
sfEqa3Df7 x y z = z - (x ^ 2 + y ^ 2)

-- lorem
sfEqa3Df8 x y z = y - (x ^ 2 + z ^ 2)

-- lorem
sfEqa3Df9 x y z = x - (y ^ 2 + z ^ 2)


grad2DSF1, grad2DSF5, grad2DSF6 :: Point2D -> Vector2D
-- This is the pair of equations for the components of the 2D gradient of 2D scalar field 1.
grad2DSF1 (x, y) = (xComp, yComp)
   where xComp = ((-1) * x) * (x ^ 2 + y ^ 2) ** ((-3) / 2)
         yComp = ((-1) * y) * (x ^ 2 + y ^ 2) ** ((-3) / 2)

-- This is the pair of equations for the components of the 2D gradient of 2D scalar field 5.
grad2DSF5 (x, y) = (xComp, yComp)
   where xComp = ((-1) * x + 3) * (x ^ 2 + y ^ 2 - 6 * x + 9)  ** (-(3 / 2)) + ((-1) * x - 3) * (x ^ 2 + y ^ 2 + 6 * x + 9) ** ((-3) / 2)
         yComp =     ((-1) * y) * ((x ^ 2 + y ^ 2 + 6 * x + 9) ** ((-3) / 2) + (x ^ 2 + y ^ 2 - 6 * x + 9) ** ((-3) / 2))

-- This is the pair of equations for the components of the 2D gradient of 2D scalar field 6.
grad2DSF6 (x, y) = (xComp, yComp)
   where xComp = ((-1) * x + 3) * (x ^ 2 + y ^ 2 - 6 * x + 9)  ** (-(3 / 2)) + (x + 3) * (x ^ 2 + y ^ 2 + 6 * x + 9) ** ((-3) / 2)
         yComp =              y * ((x ^ 2 + y ^ 2 + 6 * x + 9) ** ((-3) / 2) - (x ^ 2 + y ^ 2 - 6 * x + 9) ** ((-3) / 2))


grad3DSF1, grad3DSF5, grad3DSF6 :: Point3D -> Vector3D
-- This is the set of 3 equations for the components of the 3D gradient of 3D scalar field 5.
grad3DSF1 (x, y, z) = (xComp, yComp, zComp)
   where xComp = ((-1) * x) * (x ^ 2 + y ^ 2 + z ^ 2) ** ((-3) / 2)
         yComp = ((-1) * y) * (x ^ 2 + y ^ 2 + z ^ 2) ** ((-3) / 2)
         zComp = ((-1) * z) * (x ^ 2 + y ^ 2 + z ^ 2) ** ((-3) / 2)

grad3DSF5 (x, y, z) = (xComp, yComp, zComp)
   where xComp = ((-1) * x + 3) * (x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** (-(3 / 2)) + ((-1) * x - 3) * (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2)
         yComp =       (-y) *    ((x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** ((-3) / 2) +           (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2))
         zComp =       (-z) *    ((x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** ((-3) / 2) +           (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2))

-- This is the set of 3 equations for the components of the 3D gradient of 3D scalar field 6.
grad3DSF6 (x, y, z) = (xComp, yComp, zComp)
   where xComp = ((-1) * x + 3) * (x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** (-(3 / 2)) + (x + 3) * (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2)
         yComp =       (-y) *    ((x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** ((-3) / 2) -           (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2))
         zComp =       (-z) *    ((x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** ((-3) / 2) -           (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2))

grad3DSF7 (x, y, z) = (xComp, yComp, zComp)
   where xComp = (-2) * x
         yComp = (-2) * y
         zComp = 1


--   -------- Subsection of results and outputs: filled scalar fields --------
--   -------------------------------------------------------------------------

sc2DField1, sc2DField2, sc2DField3, sc2DField4, sc2DField5, sc2DField6 :: ScalarField2D
-- These functions take the 6 defined 2D scalar fields and fill some 60 x 60 2D arrays with scalar values.
sc2DField1 = fill2DSF sfEqa2Df1 60 60 (-6) 6 (-6) 6

sc2DField2 = fill2DSF sfEqa2Df2 60 60 (-6) 6 (-6) 6

sc2DField3 = fill2DSF sfEqa2Df3 60 60 (-6) 6 (-6) 6

sc2DField4 = fill2DSF sfEqa2Df4 60 60 (-6) 6 (-6) 6

sc2DField5 = add2DSF sc2DField3 sc2DField4

sc2DField6 = add2DSF sc2DField3 (map (map negate) sc2DField4)


sc3DField1, sc3DField2, sc3DField3, sc3DField4, sc3DField5, sc3DField6 :: ScalarField3D
-- These functions take the 6 defined 3D scalar fields and fill some 20 x 20 x 20 3D arrays with scalar values.  Successive indices in each direction are scaled to 1 unit of equation space each.
sc3DField1 = fill3DSF sfEqa3Df1 10 10 10 (-5) 5 (-5) 5 (-5) 5

sc3DField2 = fill3DSF sfEqa3Df2 10 10 10 (-5) 5 (-5) 5 (-5) 5

sc3DField3 = fill3DSF sfEqa3Df3 10 10 10 (-5) 5 (-5) 5 (-5) 5

sc3DField4 = fill3DSF sfEqa3Df4 10 10 10 (-5) 5 (-5) 5 (-5) 5

sc3DField5 = addsc3DFields sc3DField3 sc3DField4

sc3DField6 = addsc3DFields sc3DField3 (map (map (map negate)) sc3DField4)

sc3DField7 = fill3DSF sfEqa3Df7 10 10 10 (-3) 3 (-3) 3 0 9


--   -------- Subsection of results and outputs: some P lists for multi-plots (scalar field + field lines + equipotentials) --------
--   -------------------------------------------------------------------------------------------------------------------------------

-- This is the P list for 3D scalar field 6 (the dipole) plotted to Minecraft with clump size 1 and space size 7 and rainbow 1.  At 8 minecraft meters to 1 unit of equation space and 1 unit of equation space to successive values and 10 values in each direction, the plot is 80 x 80 x 80 Minecraft meters from (0, 0, 0) to (80, 80, 80).
dipole3DSFPList :: Plist
dipole3DSFPList = movePListUp dipolePList 100
   where dipolePList = scaleUListEqualColors dipoleUList 1
         dipoleUList = sf3DtoUList sc3DField6 1 7


dipoleEqpsAllLists, dipoleEqps1PList, dipoleEqps2PList, dipoleEqps3PList, dipoleEqps4PList :: Plist
-- These are the P lists for some equipotential surfaces for 3D scalar field 6 (the dipole) with -5 to 5 of x, y, and z range of equation units plotted (0, 0, 0) to (80, 80, 80) in Minecraft meters.
dipoleEqpsAllLists = movePListUp (concat [dipoleEqps1PList, dipoleEqps2PList, dipoleEqps3PList, dipoleEqps4PList]) 100

dipoleEqps1PList = eqpSurfs3D sfEqa3Df6 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
   where eqpSurfVals = [(0.6,  0.03,  11), (-0.6,  0.03,   1)]

dipoleEqps2PList = eqpSurfs3D sfEqa3Df6 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
   where eqpSurfVals = [(0.34, 0.03,  10), (-0.34, 0.03,   2)]

dipoleEqps3PList = eqpSurfs3D sfEqa3Df6 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
   where eqpSurfVals = [(0.07, 0.0011, 8), (-0.07, 0.0011, 4)]

dipoleEqps4PList = eqpSurfs3D sfEqa3Df6 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
   where eqpSurfVals = [(0.00, 0.004,  6)]


dipoleFieldLinesPList1, dipoleFieldLinesPList2 :: Plist
-- These are the P lists for some field lines for 3D scalar field 6 (the dipole) with x, y, and z scales 8.0 each and x, y, and z offsets 5.0 each, all 15 wool colors, and with the cutoff being +/- 9 for x value in equation space or getting close to a pole.
dipoleFieldLinesPList1 = movePListUp fieldLines1PList 100
   where fieldLines1PList = fieldLines3D grad3DSF6 startFieldPointsColors1 8.0 8.0 8.0 5.0 5.0 5.0 keepCondition 1.0 0.1 False
         keepCondition = \(xi, yi, zi, mi) -> mi < 30 && abs xi < 9
         startFieldPointsColors1 = zip startFieldPoints1 randColors
            where startFieldPoints1 = map (\(x, y, z) -> (x - 3, y, z)) (phiThetaArray 0.2 5 8 True)

dipoleFieldLinesPList2 = movePListUp fieldLines2PList 100
   where fieldLines2PList = fieldLines3D grad3DSF6 startFieldPointsColors2 8.0 8.0 8.0 5.0 5.0 5.0 keepCondition 1.0 0.1 True
         keepCondition = \(xi, yi, zi, mi) -> mi < 30 && abs xi < 9
         startFieldPointsColors2 = zip startFieldPoints2 (drop 1000 randColors)
            where startFieldPoints2 = map (\(x, y, z) -> (x + 3, y, z)) (phiThetaArray 0.2 5 8 True)


-- This is the P list for 3D scalar field 5 (the homo-dipole) plotted to Minecraft with clump size 1 and space size 7 and rainbow 1.  At 8 minecraft meters to 1 unit of equation space and 1 unit of equation space to successive values and 10 values in each direction, the plot is 80 x 80 x 80 Minecraft meters from (0, 0, 0) to (80, 80, 80).
homoDipole3DSFPList :: Plist
homoDipole3DSFPList = movePListUp homoDipolePList 100
   where homoDipolePList = scaleUListEqualColors homoDipoleUList 1
         homoDipoleUList = sf3DtoUList sc3DField5 1 7


homoDipoleEqpsAllLists, homoDipoleEqps1PList, homoDipoleEqps2PList :: Plist
-- These are the P lists for some equipotential surfaces for 3D scalar field 5 (the homo-dipole) with -5 to 5 of x, y, and z range of equation units plotted (0, 0, 0) to (80, 80, 80) in Minecraft meters.
homoDipoleEqpsAllLists = movePListUp (homoDipoleEqps1PList ++ homoDipoleEqps2PList) 100

homoDipoleEqps1PList = eqpSurfs3D sfEqa3Df5 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
   where eqpSurfVals = [(0.34, 0.0012,  3)]

homoDipoleEqps2PList = eqpSurfs3D sfEqa3Df5 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
   where eqpSurfVals = [(0.64, 0.001, 11)]


homoDipoleFieldLinesPList1, homoDipoleFieldLinesPList2 :: Plist
-- These are the P lists for some field lines for 3D scalar field 5 (the homo-dipole) with x, y, and z scales 8.0 each and x, y, and z offsets 5.0 each, all 15 wool colors, and with the cutoff being +/- 9 for x value and +/- 15 for y and z values in equation space.
homoDipoleFieldLinesPList1 = movePListUp fieldLines1PList 100
   where fieldLines1PList = fieldLines3D grad3DSF5 startFieldPointsColors1 8.0 8.0 8.0 5.0 5.0 5.0 keepCondition 1.0 0.1 True
         keepCondition = \(xi, yi, zi, mi) -> abs xi < 9 && abs yi < 15 && abs zi < 15
         startFieldPointsColors1 = zip startFieldPoints1 randColors
            where startFieldPoints1 = map (\(x, y, z) -> (x - 3, y, z)) (filterOutPosxPole (phiThetaArray 0.2 5 8 True))
                  filterOutPosxPole = filter (\(x, y, z) -> not(x > 0 && abs y < 0.0000001 && abs z < 0.0000001))

homoDipoleFieldLinesPList2 = movePListUp fieldLines2PList 100
   where fieldLines2PList = fieldLines3D grad3DSF5 startFieldPointsColors2 8.0 8.0 8.0 5.0 5.0 5.0 keepCondition 1.0 0.1 True
         keepCondition = \(xi, yi, zi, mi) -> abs xi < 9 && abs yi < 15 && abs zi < 15
         startFieldPointsColors2 = zip startFieldPoints2 (drop 1000 randColors)
            where startFieldPoints2 = map (\(x, y, z) -> (x + 3, y, z)) (filterOutNegxPole (phiThetaArray 0.2 5 8 True))
                  filterOutNegxPole = filter (\(x, y, z) -> not(x < 0 && abs y < 0.0000001 && abs z < 0.0000001))


dipoleFieldLinesBigStepPList1, dipoleFieldLinesBigStepPList2 :: Plist
-- These are the P lists for some field lines for 3D scalar field 6 (the dipole) with x, y, and z scales 8.0 each and x, y, and z offsets 5.0 each, all 15 wool colors, and with the cutoff being +/- 9 for x value in equation space or getting close to a pole.  The step size is big and opposing pairs of field lines are up to 1 Minecraft meter disparate.
dipoleFieldLinesBigStepPList1 = movePListUp fieldLines1PList 100
   where fieldLines1PList = fieldLines3D grad3DSF6 startFieldPointsColors1 8.0 8.0 8.0 5.0 5.0 5.0 keepCondition 1.0 0.43 False
         keepCondition = \(xi, yi, zi, mi) -> mi < 12 && abs xi < 9
         startFieldPointsColors1 = zip startFieldPoints1 randColors
            where startFieldPoints1 = map (\(x, y, z) -> (x - 3, y, z)) (phiThetaArray 0.3 5 8 True)

dipoleFieldLinesBigStepPList2 = movePListUp fieldLines2PList 100
   where fieldLines2PList = fieldLines3D grad3DSF6 startFieldPointsColors2 8.0 8.0 8.0 5.0 5.0 5.0 keepCondition 1.0 0.43 True
         keepCondition = \(xi, yi, zi, mi) -> mi < 12 && abs xi < 9
         startFieldPointsColors2 = zip startFieldPoints2 (drop 1000 randColors)
            where startFieldPoints2 = map (\(x, y, z) -> (x + 3, y, z)) (phiThetaArray 0.3 5 8 True)




--   -------- Subsection of results and outputs: the rest up to "go" commands --------
--   ---------------------------------------------------------------------------------

-- World 01 is 6 medium-size plots of 2D scalar fields of monopoles and dipoles.
go_w01 = sequence [sequence writeOps_w01a, sequence writeOps_w01b, sequence writeOps_w01c, sequence writeOps_w01d, sequence writeOps_w01e, sequence writeOps_w01f]

writeOps_w01a = writeOps "Plot-w01a-" $ pyStringsAPlot plotW01a 1 True
   where plotW01a = movePListWest    (movePListNorth    (movePListEast (movePListSouth pListW01a 0) 0)    440)    440
         pListW01a = scaleUListEqualColors uListW01a 1
         uListW01a = sf2DtoUList sc2DField1

writeOps_w01b = writeOps "Plot-w01b-" $ pyStringsAPlot plotW01b 1 True
   where plotW01b = movePListWest    (movePListNorth    (movePListEast (movePListSouth pListW01b 70) 0)    440)    440
         pListW01b = scaleUListEqualColors uListW01b 1
         uListW01b = sf2DtoUList sc2DField2

writeOps_w01c = writeOps "Plot-w01c-" $ pyStringsAPlot plotW01c 1 True
   where plotW01c = movePListWest    (movePListNorth    (movePListEast (movePListSouth pListW01c 0) 70)    440)    440
         pListW01c = scaleUListEqualColors uListW01c 1
         uListW01c = sf2DtoUList sc2DField3

writeOps_w01d = writeOps "Plot-w01d-" $ pyStringsAPlot plotW01d 1 True
   where plotW01d = movePListWest    (movePListNorth    (movePListEast (movePListSouth pListW01d 70) 70)    440)    440
         pListW01d = scaleUListEqualColors uListW01d 1
         uListW01d = sf2DtoUList sc2DField4

writeOps_w01e = writeOps "Plot-w01e-" $ pyStringsAPlot plotW01e 1 True
   where plotW01e = movePListWest    (movePListNorth    (movePListEast (movePListSouth pListW01e 0) 140)    440)    440
         pListW01e = scaleUListEqualColors uListW01e 1
         uListW01e = sf2DtoUList sc2DField5

writeOps_w01f = writeOps "Plot-w01f-" $ pyStringsAPlot plotW01f 1 True
   where plotW01f = movePListWest    (movePListNorth    (movePListEast (movePListSouth pListW01f 70) 140)    440)    440
         pListW01f = scaleUListEqualColors uListW01f 1
         uListW01f = sf2DtoUList sc2DField6


-- World 02 is a large plot of the 2D scalar field of a monopole.
go_w02 = sequence writeOps_w02

writeOps_w02 = writeOps "Plot-w02-" $ pyStringsAPlot plotW02 2 True
   where plotW02 = movePListWest (movePListNorth (scaleUListEqualColors uListW02 2) 470)240
         uListW02 = sf2DtoUList sf2DW02
         sf2DW02 = fill2DSF sfEqa2Df1 200 200 (-6) 6 (-6) 6


-- World 03 is a large plot of the 2D scalar field of a dipole, plus some field lines and equipotential curves.
go_w03 = sequence [sequence writeOps_w03a, sequence writeOps_w03b, sequence writeOps_w03c, sequence writeOps_w03d]

writeOps_w03a = writeOps "Plot-w03a-" $ pyStringsAPlot pListW03a 2 True
   where pListW03a = movePListWest (movePListNorth (scaleUListEqualColors uListW03a 2) 300) 450
         uListW03a = sf2DtoUList sf2DW03a
         sf2DW03a = fill2DSF sfEqa2Df6 200 200 (-6) 6 (-6) 6

writeOps_w03b = writeOps "Plot-w03b-" $ pyStringsAPlot eqPoCrvPList 1 True
   where eqPoCrvPList = movePListWest (movePListNorth (eqPoCrvs2D sfEqa2Df6 curveValues 200 200 (-6) 6 (-6) 6) 300) 450
         curveValues = [(0.750, 0.04, 10), (-0.750, 0.04, 2), (0.217, 0.016, 11), (-0.217, 0.016, 3), (0.040, 0.013, 8), (-0.040, 0.013, 5)]

writeOps_w03c = writeOps "Plot-w03c-" $ pyStringsAPlot fieldLinesPList 1 True
   where fieldLinesPList = movePListWest (movePListNorth (fieldLines2D grad2DSF6 startFieldPointsColors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 keepCondition 1.0 0.1 False) 300) 450
            where keepCondition = \(xi, yi, mi) -> mi < 30 && abs xi < 9
                  startFieldPointsColors1 = zip startFieldPoints1 randColors
                     where startFieldPoints1 = map (\(x, y) -> (x - 3, y)) (thetaArray 0.2 12)

writeOps_w03d = writeOps "Plot-w03d-" $ pyStringsAPlot fieldLinesPList 1 True
   where fieldLinesPList = movePListWest (movePListNorth (fieldLines2D grad2DSF6 startFieldPointsColors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 keepCondition 1.0 0.1 True) 300) 450
            where keepCondition = \(xi, yi, mi) -> mi < 30 && abs xi < 9
                  startFieldPointsColors1 = zip startFieldPoints1 randColors
                     where startFieldPoints1 = map (\(x, y) -> (x + 3, y)) (thetaArray 0.2 12)


-- World 04 is a large plot of the 2D scalar field of a homo-dipole, plus some field lines and equipotential curves.
go_w04 = sequence [sequence writeOps_w04a, sequence writeOps_w04b, sequence writeOps_w04c, sequence writeOps_w04d]

writeOps_w04a = writeOps "Plot-w04a-" $ pyStringsAPlot pListW04a 2 True
   where pListW04a = movePListWest (movePListSouth (scaleUListEqualColors uListW04a 2) 10) 480
         uListW04a = sf2DtoUList sf2DW04a
         sf2DW04a = fill2DSF sfEqa2Df5 200 200 (-6) 6 (-6) 6

writeOps_w04b = writeOps "Plot-w04b-" $ pyStringsAPlot eqPoCrvPList 1 True
   where eqPoCrvPList = movePListWest (movePListSouth (eqPoCrvs2D sfEqa2Df5 curveValues 200 200 (-6) 6 (-6) 6) 10) 480
         curveValues = [(0.920, 0.03, 11), (0.650, 0.014, 5), (0.420, 0.007, 1)]

writeOps_w04c = writeOps "Plot-w04c-" $ pyStringsAPlot fieldLinesPList 1 True
   where fieldLinesPList = movePListWest (movePListSouth (fieldLines2D grad2DSF5 startFieldPointsColors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 keepCondition 1.0 0.1 True) 10) 480
            where keepCondition = \(xi, yi, mi) -> abs xi < 9 && abs yi < 9
                  startFieldPointsColors1 = zip startFieldPoints1 randColors
                     where startFieldPoints1 = map (\(x, y) -> (x - 3, y)) (filterOutPosxPole (thetaArray 0.2 12))
                           filterOutPosxPole = filter (\(x, y) -> not(x > 0 && abs y < 0.0000001))

writeOps_w04d = writeOps "Plot-w04d-" $ pyStringsAPlot fieldLinesPList 1 True
   where fieldLinesPList = movePListWest (movePListSouth (fieldLines2D grad2DSF5 startFieldPointsColors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 keepCondition 1.0 0.1 True) 10) 480
            where keepCondition = \(xi, yi, mi) -> abs xi < 9 && abs yi < 9
                  startFieldPointsColors1 = zip startFieldPoints1 randColors
                     where startFieldPoints1 = map (\(x, y) -> (x + 3, y)) (filterOutNegxPole (thetaArray 0.2 12))
                           filterOutNegxPole = filter (\(x, y) -> not(x < 0 && abs y < 0.0000001))


-- World 05 is a bumpy sphere of one color.
go_w05 = sequence writeOps_w05

writeOps_w05 = writeOps "Plot-w05-" $ pyStringsAPlot pListW05 1 True
   where pListW05 = movePListEast (movePListNorth (movePListUp (bumpySphere 80 0.2 4 8 600 600 11) 127) 515) 385


-- World 06 is a bumpy sphere of rainbow colors.
go_w06 = sequence writeOps_w06

writeOps_w06 = writeOps "Plot-w06-" $ pyStringsAPlot pListW06 2 True
   where pListW06 = movePListEast (movePListNorth (movePListUp (bumpySphereRainbow 80 0.2 4 9 600 600 2) 127) 360) 60


-- World 7 has all the levels of the sneezing demon fractal that will fit in Minecraft space (Minecraft y range is 0 to 255).
go_w07 = sequence writeOps_w07

writeOps_w07 = writeOps "Plot-w07-" $ pyStringsAPlot pListW07 1 True
   where pListW07 = movePListWest (movePListNorth (concat [lev1Part, lev2Part, lev3Part, lev4Part, lev5Part, lev6Part, lev7Part, lev8Part]) 260) 125
         lev1Part = movePListUp (sdFractal 1 1) 100
         lev2Part = movePListSouth (movePListUp (sdFractal 2 2) 105) 13
         lev3Part = movePListSouth (movePListUp (sdFractal 3 3) 110) 28
         lev4Part = movePListSouth (movePListUp (sdFractal 4 4) 115) 48
         lev5Part = movePListSouth (movePListUp (sdFractal 5 5) 120) 80
         lev6Part = movePListSouth (movePListUp (sdFractal 6 6) 125) 136
         lev7Part = movePListSouth (movePListUp (sdFractal 7 7) 127) 240
         lev8Part = movePListSouth (movePListUp (sdFractal 8 8) 127) 430


-- World 8 has all the levels of the Sierpinski octahedron fractal that will fit in Minecraft space (Minecraft y range is 0 to 255).
go_w08 = sequence writeOps_w08

writeOps_w08 = writeOps "Plot-w08-" $ pyStringsAPlot pListW08 1 True
   where pListW08 = movePListEast (movePListNorth (concat [lev1Part, lev2Part, lev3Part, lev4Part, lev5Part, lev6Part, lev7Part, lev8Part]) 260) 140
         lev1Part = movePListUp (sierpOct 1 11) 100
         lev2Part = movePListSouth (movePListUp (sierpOct 2 10) 105) 13
         lev3Part = movePListSouth (movePListUp (sierpOct 3 9) 110) 28
         lev4Part = movePListSouth (movePListUp (sierpOct 4 8) 115) 48
         lev5Part = movePListSouth (movePListUp (sierpOct 5 7) 120) 80
         lev6Part = movePListSouth (movePListUp (sierpOct 6 6) 125) 136
         lev7Part = movePListSouth (movePListUp (sierpOct 7 5) 127) 240
         lev8Part = movePListSouth (movePListUp (sierpOct 8 4) 127) 430


-- World 9 is a large 2D graph of a monopole with 36 radial lines.
go_w09 = sequence [sequence writeOps_w09a, sequence writeOps_w09b]

writeOps_w09a = writeOps "Plot-w09a-" $ pyStringsAPlot w09PList 2 True
   where w09PList = movePListWest (movePListNorth (scaleUListLinear w09UList 2 100 100)90)70
         w09UList = sf2DtoUList sf2DW09
         sf2DW09 = fill2DSF sfEqa2Df1 200 200 (-6) 6 (-6) 6

writeOps_w09b = writeOps "Plot-w09b-" $ pyStringsAPlot fieldLinesPList 1 True
   where fieldLinesPList = movePListWest (movePListNorth (fieldLines2D grad2DSF1 startFieldPointsColors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 keepCondition 1.0 0.1 True)90)70
            where keepCondition = \(xi, yi, mi) -> abs xi < 7 && abs yi < 7
                  startFieldPointsColors1 = zip (thetaArray 1.5 36) randColors


-- World 10 is a diple plotted with height.
go_w10 = sequence writeOps_w10

writeOps_w10 = writeOps "Plot-w10-" $ pyStringsAPlot pListW10 2 True
   where pListW10 = movePListEast (movePListNorth (scaleUListEqualColorsWHeight uListW10 2 55 3 True)300)290
         uListW10 = sf2DtoUList sf2DW10
         sf2DW10 = fill2DSF sfEqa2Df6 200 200 (-6) 6 (-6) 6


-- World 11 is a 3D dipole with 3D scalar field grid, equipotential surfaces, and field lines.
-- World 12 is a 3D dipole with equipotential surfaces, and field lines (no grid).
-- World 13 is a 3D dipole with just the field lines (no grid or equipotential surfaces).
-- World 14 is a 3D dipole with just the 3D scalar field grid (no equipotential surfaces or field lines).
go_w11 = sequence [sequence writeOpsDipole1{-}, sequence writeOpsDipole2, sequence writeOpsDipole3, sequence writeOpsDipole4-}]

writeOpsDipole1 = writeOps "w14a-" $ pyStringsAPlot (movePListEast (movePListSouth (dipole3DSFPList)105)335) 1 True

writeOpsDipole2 = writeOps "wxxx-" $ pyStringsAPlot (movePListEast (movePListNorth (dipoleEqpsAllLists)540)70) 1 True

writeOpsDipole3 = writeOps "wxxx-" $ pyStringsAPlot (movePListEast (movePListNorth (dipoleFieldLinesPList1)540)70) 1 True

writeOpsDipole4 = writeOps "wxxx-" $ pyStringsAPlot (movePListEast (movePListNorth (dipoleFieldLinesPList2)540)70) 1 True


-- World 15 is an extra-thick 3D dipole grid.
go_w15 = sequence writeOps_w15

writeOps_w15 = writeOps "Plot-w15-" $ pyStringsAPlot pListW15 2 True
   where pListW15 = movePListEast (movePListSouth (movePListUp (scaleUListEqualColors uListW15 2) 100)395)310
         uListW15 = sf3DtoUList sf3DW15 3 5
         sf3DW15 = addsc3DFields sf3DW15a (map (map (map negate)) sf3DW15b)
         sf3DW15a = fill3DSF sfEqa3Df3 14 14 14 (-5) 5 (-5) 5 (-5) 5
         sf3DW15b = fill3DSF sfEqa3Df4 14 14 14 (-5) 5 (-5) 5 (-5) 5


-- World 16 is a 3D homo-dipole with 3D scalar field grid, equipotential surfaces, and field lines.
-- World 17 is a 3D homo-dipole with equipotential surfaces, and field lines (no grid).
-- World 18 is a 3D homo-dipole with just the field lines (no grid or equipotential surfaces).
go_w16 = sequence [{-sequence writeOpsHomoDipole1, sequence writeOpsHomoDipole2, -}sequence writeOpsHomoDipole3, sequence writeOpsHomoDipole4]

writeOpsHomoDipole1 = writeOps "w52a-" $ pyStringsAPlot (movePListWest (movePListNorth (homoDipole3DSFPList)215)10) 1 True

writeOpsHomoDipole2 = writeOps "wxxx-" $ pyStringsAPlot (movePListWest (movePListSouth (homoDipoleEqpsAllLists)455)50) 1 True

writeOpsHomoDipole3 = writeOps "w52c-" $ pyStringsAPlot (movePListWest (movePListNorth (homoDipoleFieldLinesPList1)215)10) 1 True

writeOpsHomoDipole4 = writeOps "w52d-" $ pyStringsAPlot (movePListWest (movePListNorth (homoDipoleFieldLinesPList2)215)10) 1 True


-- World 19 is a negative pole 3D scalar field with 2% opacity applied to fully dense.
go_w19 = sequence writeOps_w19

writeOps_w19 = writeOps "Plot-w19-" $ pyStringsAPlot pListW20 2 True
   where pListW20 = movePListEast (movePListNorth (opacity (movePListUp (scaleUListEqualColors uListW20 2) 100) 2)290)180
         uListW20 = sf3DtoUList sf3DW20 6 0
         sf3DW20 = fill3DSF sfEqa3Df2 14 14 14 (-5) 5 (-5) 5 (-5) 5


-- World 20 is the dipole field lines with step size 0.30 to embiggen the misalignment that you get when you launch from both poles.
go_w20 = sequence writeOps_w20
   where writeOps_w20 = writeOpsW20a ++ writeOpsW20b

writeOpsW20a = writeOps "PlotW20a-" $ pyStringsAPlot (movePListWest (movePListSouth (dipoleFieldLinesBigStepPList1)320)310) 1 True

writeOpsW20b = writeOps "PlotW20b-" $ pyStringsAPlot (movePListWest (movePListSouth (dipoleFieldLinesBigStepPList2)320)310) 1 True


-- World 21 is many sliced graphs.  a, b, and c are sliced in x, y, and z, respectively.  Same with g, h, and i.  d, e, and f are field lines that intersect with a, b, and c, respectively.  For each of those, the scale in the slice direction is 5, in the other two directions 25/6, and offsets 6.0 in x, y, and z.
go_w21 = sequence [sequence writeOps_w21a, sequence writeOps_w21b, sequence writeOps_w21c, sequence writeOps_w21d, sequence writeOps_w21e, sequence writeOps_w21f, sequence writeOps_w21g, sequence writeOps_w21h, sequence writeOps_w21i]

writeOps_w21a = writeOps "PlotW21a-" $ pyStringsAPlot pListMovedW21a 2 True
   where pListMovedW21a = movePListEast (movePListNorth (movePListUp pListW21a 100)90)285
         pListW21a = scaleUListEqualColors uListW21a 2
         uListW21a = sf3DSlicedtoUList sf3DSlicedW21a 1 12
         sf3DSlicedW21a = fill3DSFSliced sfEqa3Df6 1 6 50 50 (-6.0) 6.0 (-6.0) 6.0 (-6.0) 6.0

writeOps_w21b = writeOps "PlotW21b-" $ pyStringsAPlot pListMovedW21b 2 True
   where pListMovedW21b = movePListEast (movePListNorth (movePListWest (movePListSouth (movePListUp pListW21b 100) 132) 5)90)285
         pListW21b = scaleUListEqualColors uListW21b 2
         uListW21b = sf3DSlicedtoUList sf3DSlicedW21b 2 12
         sf3DSlicedW21b = fill3DSFSliced sfEqa3Df6 2 6 50 50 (-6.0) 6.0 (-6.0) 6.0 (-6.0) 6.0

writeOps_w21c = writeOps "PlotW21c-" $ pyStringsAPlot pListMovedW21c 2 True
   where pListMovedW21c = movePListEast (movePListNorth (movePListSouth (movePListUp pListW21c 100) 274)90)285
         pListW21c = scaleUListEqualColors uListW21c 2
         uListW21c = sf3DSlicedtoUList sf3DSlicedW21c 3 12
         sf3DSlicedW21c = fill3DSFSliced sfEqa3Df6 3 6 50 50 (-6.0) 6.0 (-6.0) 6.0 (-6.0) 6.0

writeOps_w21d = writeOps "PlotW21d-" $ pyStringsAPlot pListMovedW21d 1 True
   where pListMovedW21d = movePListEast (movePListNorth (movePListUp fieldLines1PList 100 ++ movePListUp fieldLines2PList 100)90)285
         fieldLines1PList = fieldLines3D grad3DSF6 startFieldPointsColors1 5.0 (25.0 / 6.0) (25.0 / 6.0) 6.0 6.0 6.0 keepCondition 1.0 0.1 False
         fieldLines2PList = fieldLines3D grad3DSF6 startFieldPointsColors2 5.0 (25.0 / 6.0) (25.0 / 6.0) 6.0 6.0 6.0 keepCondition 1.0 0.1 True
         keepCondition = \(xi, yi, zi, mi) -> abs xi < 10 && abs yi < 15 && abs zi < 15 && abs mi < 30.0
         startFieldPointsColors1 = zip startFieldPoints1 randColors
            where startFieldPoints1 = map (\(x, y, z) -> (x - 3, y, z)) (phiThetaArray 0.2 5 8 True)
         startFieldPointsColors2 = zip startFieldPoints2 randColors
            where startFieldPoints2 = map (\(x, y, z) -> (x + 3, y, z)) (phiThetaArray 0.2 5 8 True)

writeOps_w21e = writeOps "PlotW21e-" $ pyStringsAPlot pListMovedW21e 1 True
   where pListMovedW21e = movePListEast (movePListNorth (movePListWest (movePListSouth (movePListUp fieldLines1PList 100) 132 ++ movePListSouth (movePListUp fieldLines2PList 100) 132) 5)90)285
         fieldLines1PList = fieldLines3D grad3DSF6 startFieldPointsColors1 (25.0 / 6.0) 5.0 (25.0 / 6.0) 6.0 6.0 6.0 keepCondition 1.0 0.1 False
         fieldLines2PList = fieldLines3D grad3DSF6 startFieldPointsColors2 (25.0 / 6.0) 5.0 (25.0 / 6.0) 6.0 6.0 6.0 keepCondition 1.0 0.1 True
         keepCondition = \(xi, yi, zi, mi) -> abs xi < 15 && abs yi < 10 && abs zi < 15 && abs mi < 30.0
         startFieldPointsColors1 = zip startFieldPoints1 randColors
            where startFieldPoints1 = map (\(x, y, z) -> (x - 3, y, z)) (phiThetaArray 0.2 5 8 True)
         startFieldPointsColors2 = zip startFieldPoints2 randColors
            where startFieldPoints2 = map (\(x, y, z) -> (x + 3, y, z)) (phiThetaArray 0.2 5 8 True)

writeOps_w21f = writeOps "PlotW21f-" $ pyStringsAPlot pListMovedW21f 1 True
   where pListMovedW21f = movePListEast (movePListNorth (movePListSouth (movePListUp fieldLines1PList 100) 274 ++ movePListSouth (movePListUp fieldLines2PList 100) 274)90)285
         fieldLines1PList = fieldLines3D grad3DSF6 startFieldPointsColors1 (25.0 / 6.0) (25.0 / 6.0) 5.0 6.0 6.0 6.0 keepCondition 1.0 0.1 False
         fieldLines2PList = fieldLines3D grad3DSF6 startFieldPointsColors2 (25.0 / 6.0) (25.0 / 6.0) 5.0 6.0 6.0 6.0 keepCondition 1.0 0.1 True
         keepCondition = \(xi, yi, zi, mi) -> abs xi < 15 && abs yi < 15 && abs zi < 10 && abs mi < 30.0
         startFieldPointsColors1 = zip startFieldPoints1 randColors
            where startFieldPoints1 = map (\(x, y, z) -> (x - 3, y, z)) (phiThetaArray 0.2 5 8 True)
         startFieldPointsColors2 = zip startFieldPoints2 randColors
            where startFieldPoints2 = map (\(x, y, z) -> (x + 3, y, z)) (phiThetaArray 0.2 5 8 True)

writeOps_w21g = writeOps "PlotW21g-" $ pyStringsAPlot pListMovedW21g 2 True
   where pListMovedW21g = movePListEast (movePListNorth (movePListEast (movePListUp pListW21g 100) 120)90)285
         pListW21g = scaleUListEqualColors uListW21g 2
         uListW21g = sf3DSlicedtoUList sf3DSlicedW21g 1 12
         sf3DSlicedW21g = fill3DSFSliced sfEqa3Df6 1 6 50 50 (-6.0) 6.0 (-6.0) 6.0 (-6.0) 6.0

writeOps_w21h = writeOps "PlotW21h-" $ pyStringsAPlot pListMovedW21h 2 True
   where pListMovedW21h = movePListEast (movePListNorth (movePListEast (movePListSouth (movePListUp pListW21h 100) 132) 115)90)285
         pListW21h = scaleUListEqualColors uListW21h 2
         uListW21h = sf3DSlicedtoUList sf3DSlicedW21h 2 12
         sf3DSlicedW21h = fill3DSFSliced sfEqa3Df6 2 6 50 50 (-6.0) 6.0 (-6.0) 6.0 (-6.0) 6.0

writeOps_w21i = writeOps "PlotW21i-" $ pyStringsAPlot pListMovedW21i 2 True
   where pListMovedW21i = movePListEast (movePListNorth (movePListEast (movePListSouth (movePListUp pListW21i 100) 274) 120)90)285
         pListW21i = scaleUListEqualColors uListW21i 2
         uListW21i = sf3DSlicedtoUList sf3DSlicedW21i 3 12
         sf3DSlicedW21i = fill3DSFSliced sfEqa3Df6 3 6 50 50 (-6.0) 6.0 (-6.0) 6.0 (-6.0) 6.0


-- World 22 is the peanut-shaped surface of the 3D homo-dipole plotted with more thickness.
go_w22 = sequence writeOps_w22

writeOps_w22 = writeOps "PlotW22-" $ pyStringsAPlot homoDipoleEqpsThickerList 1 True
   where homoDipoleEqpsThickerList = movePListWest (movePListSouth (movePListUp pListW22 100)410)460
         pListW22 = eqpSurfs3D sfEqa3Df5 eqpSurfVals 120 120 120 (-5) 5 (-5) 5 (-5) 5
         eqpSurfVals = [(0.658, 0.011, 3)]


-- World 23 is a 3D monopole with many radial field lines.
go_w23 = sequence [sequence writeOps_w23a, sequence writeOps_w23b]

writeOps_w23a = writeOps "PlotW23a-" $ pyStringsAPlot pListW23a 1 True
   where pListW23a = movePListWest (movePListSouth (movePListUp eqpSurfPList 60)455)370
         eqpSurfPList = eqpSurfs3D sfEqa3Df1 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
         eqpSurfVals = [(0.60,  0.03,  11)]

writeOps_w23b = writeOps "PlotW23b-" $ pyStringsAPlot pListW23b 1 True
   where pListW23b = movePListWest (movePListSouth (movePListUp fieldLines2PList 60)455)370
         fieldLines2PList = fieldLines3D grad3DSF1 startFieldPointsColors2 8.0 8.0 8.0 5.0 5.0 5.0 keepCondition 1.0 0.1 True
         keepCondition = \(xi, yi, zi, mi) -> mi < 30 && mi > 0.009
         startFieldPointsColors2 = zip startFieldPoints2 (drop 1000 randColors)
            where startFieldPoints2 = phiThetaArray 0.2 8 12 True


go_w24 = sequence [sequence writeOps_w24a, sequence writeOps_w24b, sequence writeOps_w24c]

writeOps_w24a = writeOps "25a-" $ pyStringsAPlot pListW24aMoved 2 True
   where pListW24aMoved = movePListEast (movePListNorth (movePListUp pListW24a 100)180)170
         pListW24a = scaleUListEqualColors uListW24a 2
         uListW24a = sf3DtoUList sc3DField7 1 7

writeOps_w24b = writeOps "25b-" $ pyStringsAPlot pListW24bMoved 2 True
   where pListW24bMoved = movePListEast (movePListNorth (movePListUp pListW24b 100)180)170
         pListW24b = eqpSurfs3D sfEqa3Df7 eqpSurfVals 80 80 80 (-3) 3 (-3) 3 0 9
         eqpSurfVals = [(0.000,  0.060,  8)]

writeOps_w24c = writeOps "25c-" $ pyStringsAPlot pListW23c 1 True
   where pListW23c = movePListEast (movePListNorth (movePListUp fieldLinesPList 100)180)170
         fieldLinesPList = fieldLines3D grad3DSF7 startFieldPointsColors (40.0 / 3.0) (40.0 / 3.0) (80.0 / 9.0) 3.0 3.0 0.0 keepCondition 1.0 0.1 True
         keepCondition = \(xi, yi, zi, mi) -> abs xi < 4 && abs yi < 4 && zi > (-1) && zi < 10
         startFieldPointsColors = zip startFieldPoints (drop 1000 randColors)
            where startFieldPoints = map (\(x, y, z) -> (x, y, z + 1.0)) (hThetaArray 0.2 9 8 5)

  -- lorem
go_w30 = sequence writeOps_w30

writeOps_w30 = writeOps "PlotW30-" $ pyStringsAPlot pListW30 1 True
   where pListW30 = movePListEast (movePListSouth (vectorField2DLinearScale grad2DSF6 200 200 (-5) 5 (-5) 5 13 13 0.3 0.8 2)415)110

  -- lorem
go_w40 = sequence writeOps_w40

writeOps_w40 = writeOps "PlotW40-" $ pyStringsAPlot (movePListUp pListW40 100) 1 True
   where pListW40 = movePListWest (movePListSouth (vectorField3DLinearScale grad3DSF1 110 110 110 (-5) 5 (-5) 5 (-5) 5 7 7 7 0.45 0.90 1)280)570

  -- lorem
go_w41 = sequence writeOps_w41

writeOps_w41 = writeOps "PlotW41-" $ pyStringsAPlot (movePListUp pListW40 100) 1 True
   where pListW40 = movePListWest (movePListSouth (vectorField3DLinearScale grad3DSF6 110 110 110 (-5) 5 (-5) 5 (-5) 5 7 7 7 0.45 0.90 1)415)260

go_w50 = sequence [sequence writeOps_w50a, sequence writeOps_w50b]

writeOps_w50a = writeOps "Plot-w50a-" $ pyStringsAPlot plotW50a 2 True
   where plotW50a = movePListWest (movePListSouth (scaleUListEqualColors uListW50a 2)210)90
         uListW50a = sf2DtoUList sf2DW50a
         sf2DW50a = fill2DSF sfEqa2Df7 200 200 (-6) 6 (-6) 6

writeOps_w50b = writeOps "Plot-w50b-" $ pyStringsAPlot eqPoCrvPList 1 True
   where eqPoCrvPList = movePListWest (movePListSouth (eqPoCrvs2D sfEqa2Df7 curveValues 200 200 (-6) 6 (-6) 6)210)90
         curveValues = [(4.10, 0.20, 11)]

go_w51 = sequence writeOps_w51

writeOps_w51 = writeOps "Plot-w51-" $ pyStringsAPlot pListW51Moved 2 True
   where pListW51Moved = movePListEast (movePListNorth (movePListUp pListW51 80)50)190
         pListW51 = eqpSurfs3D sfEqa3Df7 eqpSurfVals 80 80 80 (-3) 3 (-3) 3 0 9
         eqpSurfVals = [(0.000,  0.060,  8)]

go_w52 = sequence [sequence writeOpsHomoDipole1, sequence writeOpsHomoDipolex2, sequence writeOpsHomoDipole3, sequence writeOpsHomoDipole4]

writeOpsHomoDipolex2 = writeOps "w52b-" $ pyStringsAPlot homoDipoleEqpsAllListsx 1 True

homoDipoleEqpsAllListsx = movePListWest (movePListNorth (movePListUp (homoDipoleEqps1PListx ++ homoDipoleEqps2PListx) 100)215)10

homoDipoleEqps1PListx = eqpSurfs3D sfEqa3Df5 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
   where eqpSurfVals = [(0.34, 0.0008,  3)]

homoDipoleEqps2PListx = eqpSurfs3D sfEqa3Df5 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5
   where eqpSurfVals = [(0.64, 0.016, 11)]

go_w53 = sequence [sequence writeOpsw53a, sequence writeOpsw53b, sequence writeOpsw53c, sequence writeOpsw53d, sequence writeOpsw53e, sequence writeOpsw53f, sequence writeOpsw53g]

writeOpsw53a = writeOps "w53a-" $ pyStringsAPlot pList53a 4 True

writeOpsw53b = writeOps "w53b-" $ pyStringsAPlot pList53b 4 True

writeOpsw53c = writeOps "w53c-" $ pyStringsAPlot pList53c 4 True

writeOpsw53d = writeOps "w53d-" $ pyStringsAPlot pList53d 4 True

writeOpsw53e = writeOps "w53e-" $ pyStringsAPlot pList53e 4 True

writeOpsw53f = writeOps "w53f-" $ pyStringsAPlot pList53f 4 True

writeOpsw53g = writeOps "w53g-" $ pyStringsAPlot pList53g 4 True

pList53a = movePListEast (movePListNorth ([(x, y, z, 1) | x <- [31..74], y <- [82, 83], z <- [10..53]])50)350

pList53b = movePListEast (movePListNorth ([(x, 84, z, 2) | x <- [31..74], z <- [10..53]])50)350

pList53c = movePListEast (movePListNorth (oneLeg ++ movePListSouth oneLeg 39 ++ movePListEast oneLeg 39 ++ movePListSouth (movePListEast oneLeg 39) 39)50)350
   where oneLeg = [(x, y, z, 3) | x <- [31..35], y <- [72..81], z <- [10..14]]

pList53d = movePListEast (movePListNorth (thicken [(41, 85, 10, 5)] 2)50)350

pList53e = movePListEast (movePListNorth (thicken [(31, 85, 30, 5)] 2)50)350

pList53f = movePListEast (movePListNorth ([(34, 85, 31, 6), (38, 85, 31, 6), (42, 85, 13, 6), (42, 85, 17, 6), (42, 85, 20, 6), (42, 85, 23, 6), (42, 85, 27, 6)])50)350

pList53g = movePListEast (movePListNorth (thicken [(41, 85, 30, 4)] 3)50)350







go_w54 = sequence [sequence writeOpsw54a, sequence writeOpsw54b, sequence writeOpsw54c, sequence writeOpsw54d, sequence writeOpsw54e, sequence writeOpsw54f, sequence writeOpsw54g, sequence writeOpsw54h, sequence writeOpsw54i, sequence writeOpsw54j, sequence writeOpsw54k]

writeOpsw54a = writeOps "w54a-" $ pyStringsAPlot pList54a 4 True

writeOpsw54b = writeOps "w54b-" $ pyStringsAPlot pList54b 4 True

writeOpsw54c = writeOps "w54c-" $ pyStringsAPlot pList54c 4 True

writeOpsw54d = writeOps "w54d-" $ pyStringsAPlot pList54d 4 True

writeOpsw54e = writeOps "w54e-" $ pyStringsAPlot pList54e 4 True

writeOpsw54f = writeOps "w54f-" $ pyStringsAPlot pList54f 4 True

writeOpsw54g = writeOps "w54g-" $ pyStringsAPlot pList54g 4 True

writeOpsw54h = writeOps "w54h-" $ pyStringsAPlot pList54h 4 True

writeOpsw54i = writeOps "w54i-" $ pyStringsAPlot pList54i 4 True

writeOpsw54j = writeOps "w54j-" $ pyStringsAPlot pList54j 4 True

writeOpsw54k = writeOps "w54k-" $ pyStringsAPlot pList54k 4 True

pList54a = movePListWest (movePListNorth ([(x, y, z, 1) | x <- [21..59], y <- [83, 84], z <- [(-64)..(-26)]])175)240

pList54b = movePListWest (movePListNorth ([(x, 85, z, 2) | x <- [21..59], z <- [(-64)..(-26)]])175)240

pList54c = movePListWest (movePListNorth ([(x, y, -65, 7) | x <- [21..59], y <- [83..123]])175)240

pList54d = movePListWest (movePListNorth ([(60, y, z, 7) | y <- [83..123], z <- [(-64)..(-26)]])175)240

pList54e = movePListWest (movePListNorth (thicken [(58, 111, -64, 5)] 2)175)240

pList54f = movePListWest (movePListNorth (thicken [(58, 86, -44, 5)] 2)175)240

pList54g = movePListWest (movePListNorth (thicken [(43, 86, -64, 5)] 2)175)240

pList54h = movePListWest (movePListNorth ([(48, 86, -64, 6), (53, 86, -64, 6), (59, 86, -49, 6), (59, 86, -54, 6), (59, 86, -59, 6), (59, 106, -64, 6), (59, 101, -64, 6), (59, 96, -64, 6), (59, 91, -64, 6), (59, 86, -64, 6)])175)240

pList54i = movePListWest (movePListNorth (movePListSouth onexLine 21 ++ movePListUp onexLine 26 ++ movePListWest onezLine 16 ++ movePListUp onezLine 26 ++ movePListSouth oneyLine 21 ++ movePListWest oneyLine 16 ++ [(43, 86, -43, 6), (43, 112, -64, 6), (59, 112, -43, 6)])175)240
   where onexLine = [(48, 86, -64, 6), (53, 86, -64, 6)]
         onezLine = [(59, 86, -49, 6), (59, 86, -54, 6), (59, 86, -59, 6)]
         oneyLine = [(59, 106, -64, 6), (59, 101, -64, 6), (59, 96, -64, 6), (59, 91, -64, 6)]

pList54j = movePListWest (movePListNorth (movePListSouth (movePListUp [(48, 86, -64, 6), (53, 86, -64, 6)] 26) 21 ++ movePListWest (movePListUp [(59, 86, -49, 6), (59, 86, -54, 6), (59, 86, -59, 6)] 26) 16 ++ movePListSouth (movePListWest [(59, 106, -64, 6), (59, 101, -64, 6), (59, 96, -64, 6), (59, 91, -64, 6)] 16) 21)175)240

pList54k = movePListWest (movePListNorth (thicken [(42, 111, -44, 4)] 3)175)240

go_w55 = sequence [{-sequence writeOpsw55a, sequence writeOpsw55b, sequence writeOpsw55c, sequence writeOpsw55d, sequence writeOpsw55e, sequence writeOpsw55f, sequence writeOpsw55g, -}sequence writeOpsw55h]

writeOpsw55a = writeOps "w55a-" $ pyStringsAPlot pList55a 4 True

writeOpsw55b = writeOps "w55b-" $ pyStringsAPlot pList55b 2 True

writeOpsw55c = writeOps "w55c-" $ pyStringsAPlot pList55c 2 True

writeOpsw55d = writeOps "w55d-" $ pyStringsAPlot pList55d 2 True

writeOpsw55e = writeOps "w55e-" $ pyStringsAPlot pList55e 4 True

writeOpsw55f = writeOps "w55f-" $ pyStringsAPlot pList55f 4 True

writeOpsw55g = writeOps "w55g-" $ pyStringsAPlot pList55g 4 True

writeOpsw55h = writeOps "w55h-" $ pyStringsAPlot pList55h 4 True

pList55a = [(x, y, 0, 8) | x <- [(-220)..170], y <- [50..230]]

pList55b = thicken (opacity (zipWith (\(x, y, z) c -> (x, y, z, c)) [(x, y, -1) | x <- [(-220)..10], y <- [50..230]] randColors22) 2) 3

randPoints = thicken (opacitywSeed (opacitywSeed foo 2 3) 2 2) 3
   where foo = zipWith (\(x, y, z) c -> (x, y, z, c)) [(x, y, z) | x <- [(-220)..10], y <- [50..230], z <- [(-150)..150]] randColors22

pList55c = filter (\(x, y, z, c) -> z < 0) randPoints

pList55d = filter (\(x, y, z, c) -> z > 0) randPoints

pList55e = [(-79, y, z, 8) | y <- [50..230], z <- [(-195)..195]]

pList55f = thicken [(-80, y, -1, 1) | y <- [50,52..230]] 3

pList55g = [(x, 90, z, 8) | x <- [(-220)..170], z <- [(-195)..195]]

pList55h = thicken [(-80, 89, -1, 1)] 5

go_x = sequence writeOpsx
   where writeOpsx = writeOps "w53stand" $ pyStringsAPlot pListx 4 True
         pListx = [(13, 102, 40, 1)]

go_x2 = sequence writeOpsx2
   where writeOpsx2 = writeOps "w54stand" $ pyStringsAPlot pListx 4 True
         pListx = [(-5, 94, -16, 1)]

go_x3 = sequence [sequence writeOpsx2a, sequence writeOpsx2b, sequence writeOpsx2c]
   where writeOpsx2a = writeOps "w55stand1" $ pyStringsAPlot pListx2a 4 True
         writeOpsx2b = writeOps "w55stand2" $ pyStringsAPlot pListx2b 4 True
         writeOpsx2c = writeOps "w55stand3" $ pyStringsAPlot pListx2c 4 True
         pListx2a = [(-35, 125, 30, 1)]
         pListx2b = [(-35, 93, 25, 1)]
         pListx2c = [(-59, 97, 14, 1)]

-- this line is here for testing color themes lol.
commandfoo = do (do (do (do putStrLn (let foo = 5 in "AAAAAAA"))))

-- Goer.  To generate outputs from an .exe, set main to the go command for whichever world, then compile and run.
main = do
   go_x3
   return()

-- goJS is the command for a quick render in p5.js.  Change the P list input to whatever you want to render and then run this command.  Have an .html file that loads p5.js then the sketch that comes from this output and open that .html file in a web browser and you should have a render of the cubes that you can rotate by clicking and dragging.
goJS = writeFile "sketch.js" jsString
jsString = jsStringAPlot jsOutputPList 2  -- This is just an example P list for the goJS command (the quick render in p5.js).  It's the peanut-shaped graph of world 22.
   where jsOutputPList = movePListUp (eqpSurfs3D sfEqa3Df5 eqpSurfVals 80 80 80 (-5) 5 (-5) 5 (-5) 5) 80
         eqpSurfVals = [(0.658, 0.012, 22)]
