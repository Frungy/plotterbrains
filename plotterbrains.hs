--------------------------
-------- Imports ---------
--------------------------

import Data.List (nub, sort, sortBy)
import System.Random (randomR, mkStdGen)




-----------------------------------
-------- Type Definitions ---------
-----------------------------------

-- A U set is a set of co-ordinates plus an unscaled value.  Example: a set of (x, y, z) co-ordinates and the value of a 3D scalar field at that point.
type Uset = (Int, Int, Int, Float)

-- A U list is a list of U sets.
type Ulist = [Uset]

-- A P set ("set of information for a plot point") is a set of co-ordinates plus a color value.  These should be in minecraft x, y, z, and the color value should be specific to whether you're using an 11-value rainbow or a 22-value rainbow.
type Pset = (Int, Int, Int, Int)

-- A P list is a list of P sets.  All of the P sets in a given P list should have their color values specific to whether you're using an 11-color rainbow or a 22-color rainbow.
type Plist = [Pset]

-- A 2D scalar field is a 2D grid of scalar values.  This is represented as a list of lists.  Example: For temperature as a function of x and y, this data type would be a list of lists of temperature values.
type ScalarField2D = [[Float]]

-- A 3D scalar field is a 3D grid of scalar values.  This is represented as a list of lists of lists.  Example: For temperature as a function of x and y and z, this data type would be a list of lists of lists of temperature values.
type ScalarField3D = [[[Float]]]

-- Note: For 2D scalar fields and 3D scalar fields, all the dimensions should be consistent, but there's no enforcement.  If the dimensions aren't consistent then it will maybe crash with out of bounds error and maybe not.  When functions do things to these, the dimensions are tested as length of first row first column.  If any of the lists (or lists of lists, or lists of lists of lists) after first are smaller than the first, then there will be an out of bounds error.  If any are bigger, then the extra values will be ignored and there won't be a crash.

-- A point in 2D space is a pair of floats for x and y co-ordinates
type Point2D = (Float, Float)

-- A point in 3D space is a triple of floats for x, y, and z co-ordinates
type Point3D = (Float, Float, Float)

-- A vector in 2D space is a pair of floats for x and y components.
type Vector2D = (Float, Float)

-- A vector in 3D space is a triple of floats for x, y, and z components.
type Vector3D = (Float, Float, Float)




------------------------------------
-------- Bank of functions ---------
------------------------------------

-- theta array makes a circular array of 2D points around the origin.  Give it a radius and a number of points, and it gives you a list of (x, y) points.  Use some other function to move it from the center to wherever you want to put it.
thetaArray :: Float  -- radius (rho)
  -> Int  -- number of theta values (polar angles)
  -> [Point2D]
thetaArray rho numThetas = [let theta = thetas !! pred thetaInd in (x rho theta, y rho theta) | thetaInd <- thetaInds]  -- The list of all point (x, y) values is obtained by taking all theta indices and then for each one calculating the x and y values from that theta and the radius.
   where thetaInds = [1 .. numThetas]  -- Theta indices range from 1 to the input number of points.
         x rho theta = rho * sin theta  -- This is the formula to get an x value from a pair of polar co-ordinates.
         y rho theta = rho * cos theta  -- This is the formula to get a y valye from a pair of polar co-ordinates.
         thetas = map indexToAngle [1 .. numThetas]  -- The theta values are obtained by mapping some function to the list of indices (first theta, second theta, etc).
            where indexToAngle ind = 2.0 * pi * (fromIntegral ind - 1.0) / fromIntegral numThetas  -- This is the function for taking an index number for a theta and getting the theta value in radians.


-- phi theta array makes a spherical array of 3D points around the origin.  Give it a radius, a number of lines of latitude, a number of lines of longitude, and a boolean condition of whether or not to include the z-pole points, and it gives you a list of (x, y, z) points.  Use some other function to move it from the center to wherever you want to put it.
phiThetaArray :: Float  -- radius (rho)
  -> Int  -- number of phi values (elevation angles)
  -> Int  -- number of theta values (yaw angles)
  -> Bool  -- include poles in result?
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


-- fill 2D scalar field is the function for getting a 2D array of scalar values from a range of min and max x and y values, a number of slices to take in those ranges of x and y, and a function that returns a scalar value from an x and y pair.
fill2DSF :: (Float -> Float -> Float)  -- a scalar field equation for getting a scalar value from an (x, y) pair
  -> Int  -- number of points in x direction
  -> Int  -- number of points in y direction
  -> Float  -- minimum x value
  -> Float  -- maximum x value
  -> Float  -- minimum y value
  -> Float  -- maximum y value
  -> ScalarField2D
fill2DSF sfEqa xPts yPts xMin xMax yMin yMax = [ [ sfEqa (x n) (y m) | n <- ns ] | m <- ms ]  -- This applies the scalar field equation to the list of all combinations of n value (x index) and m value (y index).
   where ns = [1, 2 .. yPts]  -- This is the list of n values, which are the indices for the x direction.
         ms = [1, 2 .. xPts]  -- This is the list of m values, which are the indices for the y direction.
         xRange = xMax - xMin  -- x range is the value of the span of x values.
         yRange = yMax - yMin  -- y range is the value of the span of y values.
         dx = xRange / fromIntegral (pred xPts)  -- dx is the step size between successive x values.
         dy = yRange / fromIntegral (pred yPts)  -- dy is the step size between successive y values.
         x n = xMin + (fromIntegral (pred n)) * dx  -- This gives an x value for an x index (n) based on the range and step size for x.
         y m = yMin + (fromIntegral (pred m)) * dx  -- This gives a y value for a y index (m) based on the range and step size for x.


-- 2D scalar field equation 1 is for a monopole at the origin that gives off positive values (bigger closer).
sfEqa2Df1 :: Float -> Float -> Float
sfEqa2Df1 x y = 1 / sqrt (x ^ 2 + y ^ 2)


-- 2D scalar field equation 2 is for a monopole at the origin that gives off negative values (bigger negative closer).
sfEqa2Df2 :: Float -> Float -> Float
sfEqa2Df2 x y = negate $ sfEqa2Df1 x y


-- 2D scalar field equation 3 is for a monopole at (3, 0) that gives off positive values (bigger closer).
sfEqa2Df3 :: Float -> Float -> Float
sfEqa2Df3 x y = 1 / sqrt ((x - 3) ^ 2 + y ^ 2)


-- 2D scalar field equation 4 is for a monopole at (-3, 0) that gives off positive values (bigger closer).
sfEqa2Df4 :: Float -> Float -> Float
sfEqa2Df4 x y = 1 / sqrt ((x + 3) ^ 2 + y ^ 2)


-- 2D scalar field equation 5 is for a pair of positive poles, one at (3, 0, 0), and one at (-3, 0, 0).
sfEqa2Df5 :: Float -> Float -> Float
sfEqa2Df5 x y = sfEqa2Df3 x y + sfEqa2Df4 x y


-- 2D scalar field equation 6 is for a dipole with a positive pole at (3, 0, 0), and a negative pole at (-3, 0, 0).
sfEqa2Df6 :: Float -> Float -> Float
sfEqa2Df6 x y = sfEqa2Df3 x y - sfEqa2Df4 x y


-- add 2D scalar fields takes a pair of 2D scalar fields and returns a scalar field whose values are sums of corresponding terms of the inputs.
add2DSF :: ScalarField2D -> ScalarField2D -> ScalarField2D
add2DSF inpSF1 inpSF2 = [ [ ((inpSF1 !! pred yInd) !! pred xInd) + ((inpSF2 !! pred yInd) !! pred xInd) | xInd <- xInds] | yInd <- yInds ]  -- For each of the x indices and for each of the z indices, the corresponding entry in the result is the sum of the values found at those indices in the two inputs.
   where yDim = length inpSF1  -- y dim is the number of points in the y direction, which is the second dimension of the 2D array.  This assumes the y dim of the second input will be equal to the y dim of the first.
         xDim = length $ inpSF1 !! 0  -- x dim is the number of points in the x direction, which is the first dimension of the 2D array.  This assumes all x dims (of all x-rows of both inputs) will be equal to the size of the first x-row of the first input.
         yInds = [1, 2 .. yDim]  -- The y indices range from 1 to the number of points in the y direction.
         xInds = [1, 2 .. xDim]  -- The x indices range from 1 to the number of points in the x direction.


-- equipotential curve(s) in 2D returns a P list for one or any number of equipotential curves.  Takes one or a list of conditions (potential, tolerance, and color for each curve), a 2D scalar field equation, max bounds for x and z Minecraft co-ordinates (min for both are hard-coded to start at 1 and height is hard-coded as 100), and min and max values for x and y in equation space.  Note: before this step, we formulate the equation in terms of x and y, but then at this step, we convert (x, y, _) to (z, x, _) (because Minecraft has y for vertical).
eqPoCrvs2D :: (Float -> Float -> Float)  -- a scalar field equation for getting a scalar value from an (x, y) pair
  -> [(Float, Float, Int)]  -- list of conditions for each curve.  Each curve is defined by a value for potential, a value for tolerance, and a value for color
  -> Int  -- Minecraft x dimension max value
  -> Int  -- Minecraft z dimension max value
  -> Float  -- equation space x dimension min value
  -> Float  -- equation space x dimension max value
  -> Float  -- equation space y dimension min value
  -> Float  -- equation space y dimension max value
  -> Plist
eqPoCrvs2D sfEqa inpList mcxMax mczMax eqxMin eqxMax eqyMin eqyMax = map (\(Just a) -> a) justsAllCrvs  -- This takes all the P sets for all curves that weren't Nothings, and extracts all the values from their Justs.
   where getMaybesOneCrv (potential, tolerance, color) = [ if inBounds (x mcz) (y mcx) then Just (mcx, 101, mcz, color) else Nothing | mcx <- mcxs, mcz <- mczs ]  -- This takes the potential, tolerance, and color for one curve and returns P sets for all blocks that should be part of it.
            where mczs = [1 .. mczMax]  -- Minecraft z values range from 1 to input max z value.
                  mcxs = [1 .. mcxMax]  -- Minecraft x values range from 1 to input max x value.
                  x mcz = (fromIntegral mcz) * (eqxMax - eqxMin) / (fromIntegral mczMax) + eqxMin  -- This takes a Minecraft z value and maps it to the x in the equation space.
                  y mcx = (fromIntegral mcx) * (eqyMax - eqyMin) / (fromIntegral mcxMax) + eqyMin  -- This takes a Minecraft x value and maps it to the y in the equation space.
                  difference x z = sfEqa x z - potential  -- This takes an x and z in equation space, finds the scalar field value there, and compares it to the target value for one curve.
                  inBounds x z = abs (difference x z) < tolerance  -- This takes an x and z in equation space, and finds whether the scalar field value there is within the tolerance of the target value for one curve.
         maybesAllCrvs = concatMap getMaybesOneCrv inpList  -- This takes the function that returns P sets for one curve and applies it to all the curves from the input.
         justsAllCrvs = filter (/= Nothing) maybesAllCrvs  -- This takes the P sets for all curves and filters out all the Nothings.


-- field lines 2D returns a list of Minecraft block locations and colors based on stepping along a gradient function in 2D.  The output should plot as a set of curves.  It takes an equation for computing a 2D gradient, and a list of starting locations and colors, and generates the curves by stepping along that gradient to turn each starting location into a curve.  It needs values for converting from the equation space to the Minecraft space (scales and offsets), a condition to determine how to cut off a curve, a step size, and a fake starting magnitude for each curve.
-- TODO: change floor to round???
-- TODO: change from zipWith to map?
-- TODO: off by one matching mags with locations?
fieldLines2D :: (Point2D -> Vector2D)  -- an equation that takes a 2D point and returns a 2D vector for the gradient there
  -> [(Point2D, Int)]  -- the input list of 2D starting points, each of which will become a curve, and the color for each curve / starting point
  -> Float  -- the factor for converting from equation space x to Minecraft space z
  -> Float  -- the factor for converting from equation space y to Minecraft space x
  -> Float  -- offset value for comparing the equation space's origin x to the Minecraft space's origin z
  -> Float  -- offset value for comparing the equation space's origin y to the Minecraft space's origin x
  -> ((Float, Float, Float) -> Bool)  -- a condition that takes a 2D point and magnitude and determines whether it's not beyond the threshold for keeping (e.g. magnitude is low enough and point is within the graph)
  -> Float  -- a fake starting magnitude to pair with each curve's starting point
  -> Float  -- the step size in Minecraft meters
  -> Bool  -- a condition for reversing the polarity or not
  -> Plist
fieldLines2D gradEq inpList scale_dim1 scale_dim2 offset_dim1 offset_dim2 keepCond fakeStartMag stepSize reverseYN = thicken (concatMap plist1c inpListwFakeStartMags) 2  -- This takes all the tuples for a curve's starting location, color, and fake start magnitude, computes the Minecraft block locations and colors for each, flattens the results, and thickens those.
   where inpListwFakeStartMags = map (\((x, y), c) -> ((x, y, fakeStartMag), c)) inpList  -- This takes the input start points and colors and adds the fake starting magnitude to each tuple.
         nextLoc (xi, yi, mi) = if not reverseYN then (xi + dx, yi + dy, magGrad) else (xi + negate dx, yi + negate dy, magGrad)  -- This takes a set of values for x and y and magnitude there and generates the x and y and magnitude of the next point.
            where (grad_x, grad_y) = gradEq (xi, yi)  -- This returns the x and y components of the 2D gradient at a point.
                  magGrad = sqrt (grad_x ** 2.0 + grad_y ** 2.0)  -- This returns the magnitude of the 2D gradient at a point.
                  dx = grad_x / magGrad * stepSize  -- This returns the step size in the equation x direction by normalizing the gradient, multiplying that unit vector by the step size, and taking the x component.
                  dy = grad_y / magGrad * stepSize  -- This returns the step size in the equation y direction by normalizing the gradient, multiplying that unit vector by the step size, and taking the y component.
         infinLocs1c startLoc = iterate nextLoc startLoc  -- This returns an infinitely long list of locations by taking some starting location and taking the next step an infinite number of times.
         keeperLocs1c startLoc = takeWhile keepCond (infinLocs1c startLoc)  -- This returns a finitely long list of locations by taking some staring location, generating an infinitely long list of next step values, and then cutting off the list after the keep condition from the input.
         eq_xy_to_MC_xyz (ex, ey, _) = (mcx, mcy, mcz)  -- This maps a point in the equation (x, y) to Minecraft (x, y, z) (by (x, y, _) -> (z, x, y)), using a MC y height of 100 for a level plane.
            where mcx = floor ((ey + offset_dim2) * scale_dim2)  -- In this conversion process, a Minecraft x value is based on an equation y value and the dim 2 offset and scale values from the input.
                  mcy = 101  -- The altitude of level plane in Minecraft y.
                  mcz = floor ((ex + offset_dim1) * scale_dim1)  -- In this conversion process, a Minecraft z value is based on an equation x value and the dim 1 offset and scale values from the input.
         mckeeperLocs1c startLoc = nub $ map eq_xy_to_MC_xyz $ keeperLocs1c startLoc  -- This takes a starting location, gets the list of points that satisfy the keep condition based on stepping from there, converts to Minecraft space, and removes duplicates.
         plist1c (startLoc, color) = zipWith (\(x, y, z) c -> (x, y, z, c)) (mckeeperLocs1c startLoc) (repeat color)  -- This takes the list of Minecraft block locations (with duplicates removed) and tuples the color for that curve with each block.


-- This is the pair of equations for the components of the 2D gradient of 2D scalar field 5.
grad2DSF5 :: Point2D -> Vector2D
grad2DSF5 (x, y) = (xComp, yComp)
   where xComp = ((-1) * x + 3) * (x ^ 2 + y ^ 2 - 6 * x + 9) ** (-(3 / 2)) + ((-1) * x - 3) * (x ^ 2 + y ^ 2 + 6 * x + 9) ** ((-3) / 2)
         yComp =     ((-1) * y) * ((x ^ 2 + y ^ 2 + 6 * x + 9) ** ((-3) / 2) + (x ^ 2 + y ^ 2 - 6 * x + 9) ** ((-3) / 2))

-- This is the pair of equations for the components of the 2D gradient of 2D scalar field 6.
grad2DSF6 :: Point2D -> Vector2D
grad2DSF6 (x, y) = (xComp, yComp)
   where xComp = ((-1) * x + 3) * (x ^ 2 + y ^ 2 - 6 * x + 9) ** (-(3 / 2)) + (x + 3) * (x ^ 2 + y ^ 2 + 6 * x + 9) ** ((-3) / 2)
         yComp =              y * ((x ^ 2 + y ^ 2 + 6 * x + 9) ** ((-3) / 2) - (x ^ 2 + y ^ 2 - 6 * x + 9) ** ((-3) / 2))


-- fill 3D scalar field is the function for getting a 3D array of scalar values from a range of min and max x, y, and z values, a number of slices to take in those ranges of x, y, and z, and a function that returns a scalar value from an (x, y, z) triple.
fill3DSF :: (Float -> Float -> Float -> Float)  -- a scalar field equation for getting a scalar value from an (x, y, z) triple.
  -> Int  -- number of points in the x direction
  -> Int  -- number of points in the y direction
  -> Int  -- number of points in the z direction
  -> Float  -- minimum x value
  -> Float  -- maximum x value
  -> Float  -- minimum y value
  -> Float  -- maximum y value
  -> Float  -- minimum z value
  -> Float  -- maximum z value
  -> ScalarField3D
fill3DSF sfEqa xPts yPts zpts xMin xMax yMin yMax zMin zMax = [ [ [ sfEqa (x m) (y n) (z o) | m <- ms ] | n <- ns ] | o <- os ]
   where ms = [1, 2 .. xPts]  -- This is the list of m values, which are the indices for the x direction.
         ns = [1, 2 .. yPts]  -- This is the list of n values, which are the indices for the y direction.
         os = [1, 2 .. zpts]  -- This is the list of o values, which are the indices for the z direction.
         xRange = xMax - xMin  -- x range is the value of the span of x values.
         yRange = yMax - yMin  -- y range is the value of the span of y values.
         zrange = zMax - zMin  -- z range is the value of the span of z values.
         dx = xRange / fromIntegral (pred xPts)  -- dx is the step size between successive x values.
         dy = yRange / fromIntegral (pred yPts)  -- dy is the step size between successive y values.
         dz = zrange / fromIntegral (pred zpts)  -- dz is the step size between successive z values.
         x m = xMin + (fromIntegral (pred m)) * dx  -- This gives an x value for an x index (m) based on the range and step size for x.
         y n = yMin + (fromIntegral (pred n)) * dy  -- This gives a y value for a y index (n) based on the range and step size for y.
         z o = zMin + (fromIntegral (pred o)) * dz  -- This gives a z value for a z index (o) based on the range and step size for z.


-- 3D scalar field equation 1 is for a monopole at the origin that gives off positive values (bigger closer).
sfEqa3Df1 :: Float -> Float -> Float -> Float
sfEqa3Df1 x y z = 1 / sqrt (x ^ 2 + y ^ 2 + z ^ 2)

-- 3D scalar field equation 2 is for a monopole at the origin that gives off negative values (bigger negative closer).
sfEqa3Df2 :: Float -> Float -> Float -> Float
sfEqa3Df2 x y z = negate $ sfEqa3Df1 x y z

-- 3D scalar field equation 3 is for a monopole at (3, 0, 0) that gives off positive values (bigger closer).
sfEqa3Df3 :: Float -> Float -> Float -> Float
sfEqa3Df3 x y z = 1 / sqrt ((x - 3) ^ 2 + y ^ 2 + z ^ 2)

-- 3D scalar field equation 4 is for a monopole at (-3, 0, 0) that gives off positive values (bigger closer).
sfEqa3Df4 :: Float -> Float -> Float -> Float
sfEqa3Df4 x y z = 1 / sqrt ((x + 3) ^ 2 + y ^ 2 + z ^ 2)

-- 3D scalar field equation 11 is for a dipole with a positive pole at (3, 0, 0) and a negative pole at (-3, 0, 0).
sfEqa3Df11 :: Float -> Float -> Float -> Float
sfEqa3Df11 x y z = sfEqa3Df3 x y z - sfEqa3Df4 x y z


-- add 3D scalar fields takes a pair of 3D scalar fields and returns a scalar field whose values are sums of corresponding terms of the inputs.
addsc3Dfields :: ScalarField3D -> ScalarField3D -> ScalarField3D
addsc3Dfields inpSF1 inpSF2 = [ [ [ (((inpSF1 !! pred zInd) !! pred yind) !! pred xInd) + (((inpSF2 !! pred zInd) !! pred yind) !! pred xInd) | xInd <- xInds] | yind <- yInds] | zInd <- zInds ]
   where zDim = length inpSF1  -- z dim is the number of points in the z direction, which is the third dimension of the 3D array.  This assumes the z dim of the second input will be equal to the z dim of the first.
         yDim = length $ inpSF1 !! 0  -- y dim is the number of points in the y direction, which is the second dimension of the 3D array.  This assumes all y dims (of all y-rows of both inputs) will be equal to the size of the first y-row of the first input.
         xDim = length $ (inpSF1 !! 0) !! 0  -- x dim is the number of points in the x direction, which is the first dimension of the 3D array.  This assumes all x dims (of all x-rows of both inputs) will be equal to the size of the first x-row of the first input.
         zInds = [1, 2 .. zDim]  -- The z indices range from 1 to the number of points in the z direction.
         yInds = [1, 2 .. yDim]  -- The y indices range from 1 to the number of points in the y direction.
         xInds = [1, 2 .. xDim]  -- The x indices range from 1 to the number of points in the x direction.


-- equipotential surfaces(s) in 3D returns a P list for one or any number of equipotential curves.  Takes one or a list of conditions (potential, tolerance, and color for each curve), a 3D scalar field equation, max bounds for x, y, and z Minecraft co-ordinates (min for all are hard-coded to start at 1), and min and max values for x, y, and z in equation space.  Note: before this step, we formulate the equation in terms of (x, y, z), but then at this step, we convert (x, y, z) to (z, x, y) (because Minecraft has y for vertical).
eqpSurfs3D :: (Float -> Float -> Float -> Float)  -- an equation that takes a 3D point and returns a 3D vector for the gradient there
  -> [(Float, Float, Int)]  -- list of conditions for each surface.  Each surface is defined by a value for potential, a value for tolerance, and a value for color
  -> Int  -- Minecraft x dimension max value
  -> Int  -- Minecraft y dimension max value
  -> Int  -- Minecraft z dimension max value
  -> Float  -- equation space x dimension min value
  -> Float  -- equation space x dimension max value
  -> Float  -- equation space y dimension min value
  -> Float  -- equation space y dimension max value
  -> Float  -- equation space z dimension min value
  -> Float  -- equation space z dimension max value
  -> Plist
eqpSurfs3D sfEqa inpList mcxMax mcyMax mczMax eqxMin eqxMax eqyMin eqyMax eqzMin eqzMax = map (\(Just a) -> a) justsAllSurfs  -- This takes all the P sets for all surfaces that weren't Nothings, and extracts all the values from their Justs.
   where getMaybesOneSurf (potential, tolerance, color) = [ if inBounds (x mcz) (y mcx) (z mcy) then Just (mcx, mcy, mcz, color) else Nothing | mcx <- mcxs, mcy <- mcys, mcz <- mczs ]  -- This takes the potential, tolerance, and color for one surface and returns P sets for all blocks that should be part of it.
            where mcxs = [1 .. mcxMax]  -- Minecraft x values range from 1 to input max x value.
                  mcys = [1 .. mcyMax]  -- Minecraft y values range from 1 to input max y value.
                  mczs = [1 .. mczMax]  -- Minecraft z values range from 1 to input max z value.
                  x mcz = (fromIntegral mcz) * (eqxMax - eqxMin) / (fromIntegral mczMax) + eqxMin  -- This takes a Minecraft z value and maps it to the x in the equation space.
                  y mcx = (fromIntegral mcx) * (eqyMax - eqyMin) / (fromIntegral mcxMax) + eqyMin  -- This takes a Minecraft x value and maps it to the y in the equation space.
                  z mcy = (fromIntegral mcy) * (eqzMax - eqzMin) / (fromIntegral mcyMax) + eqzMin  -- This takes a Minecraft y value and maps it to the z in the equation space.
                  difference x y z = sfEqa x y z - potential  -- This takes an x, y, and z in equation space, finds the scalar field value there, and compares it to the target value for one curve.
                  inBounds x y z = abs (difference x y z) < tolerance  -- This takes an x, y, and z in equation space, and finds whether the scalar field value there is within the tolerance of the target value for one curve.
         maybesAllSurfs = concatMap getMaybesOneSurf inpList  -- This takes the function that returns P sets for one surface and applies it to all the surfaces from the input.
         justsAllSurfs = filter (/= Nothing) maybesAllSurfs  -- This takes the P sets for all surfaces and filters out all the Nothings.


-- field lines 3D returns a list of Minecraft block locations and colors based on stepping along a gradient function in 3D.  The output should plot as a set of curves.  It takes an equation for computing a 3D gradient, and a list of starting locations and colors, and generates the curves by stepping along that gradient to turn each starting location into a curve.  It needs values for converting from the equation space to the Minecraft space (scales and offsets), a condition to determine how to cut off a curve, a step size, and a fake starting magnitude for each curve.
-- TODO: how was this not sideways before (in prog 12)??  Did I just turn it sideways in trying to fix it??
fieldLines3D :: (Point3D -> Vector3D)  -- an equation that takes a 3D point and returns a 3D vector for the gradient there
  -> [(Point3D, Int)]  -- the input list of 3D starting points, each of which will become a curve, and the color for each curve / starting point
  -> Float  -- the factor for converting from equation space x to Minecraft space z
  -> Float  -- the factor for converting from equation space y to Minecraft space x
  -> Float  -- the factor for converting from equation space z to Minecraft space y
  -> Float  -- offset value for comparing the equation space's origin x to the Minecraft space's origin z
  -> Float  -- offset value for comparing the equation space's origin y to the Minecraft space's origin x
  -> Float  -- offset value for comparing the equation space's origin z to the Minecraft space's origin y
  -> ((Float, Float, Float, Float) -> Bool)  -- a condition that takes a 3D point and magnitude and determines whether it's not beyond the threshold for keeping (e.g. magnitude is low enough and point is within the graph)
  -> Float  -- a fake starting magnitude to pair with each curve's starting point
  -> Float  -- the step size in Minecraft meters
  -> Bool  -- a condition for reversing the polarity or not
  -> Plist
fieldLines3D gradEq inpList scale_dim1 scale_dim2 scale_dim3 offset_dim1 offset_dim2 offset_dim3 keepCond fakeStartMag stepSize reverseYN = thicken (concatMap plist1c inpListwFakeStartMags) 2  -- This takes all the tuples for a curve's starting location, color, and fake start magnitude, computes the Minecraft block locations and colors for each, flattens the results, and thickens those.
   where inpListwFakeStartMags = map (\((x, y, z), c) -> ((x, y, z, fakeStartMag), c)) inpList  -- This takes the input start points and colors and adds the fake starting magnitude to each tuple.
         nextLoc (xi, yi, zi, mi) = if not reverseYN then (xi + dx, yi + dy, zi + dz, magGrad) else (xi + negate dx, yi + negate dy, zi + negate dz, magGrad)  -- This takes a set of values for x, y, z, and magnitude there and generates the x, y, z, and magnitude of the next point.
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
         plist1c (startLoc, color) = zipWith (\(x, y, z) c -> (x, y, z, c)) (mckeeperLocs1c startLoc) (repeat color)  -- This takes the list of Minecraft block locations (with duplicates removed) and tuples the color for that curve with each block.


-- This is the set of 3 equations for the components of the 3D gradient of 3D scalar field 11.
grad3DSF11 :: Point3D -> Vector3D
grad3DSF11 (x, y, z) = (xComp, yComp, zComp)
   where xComp = ((-1) * x + 3) * (x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** (-(3 / 2)) + (x + 3) * (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2)
         yComp =       (-y) *    ((x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** ((-3) / 2) -           (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2))
         zComp =       (-z) *    ((x ^ 2 + y ^ 2 + z ^ 2 - 6 * x + 9) ** ((-3) / 2) -           (x ^ 2 + y ^ 2 + z ^ 2 + 6 * x + 9) ** ((-3) / 2))


-- bumpy sphere makes a bumpy sphere of one color with any desired values of: mean radius, relative amplitude, number of wobbles in both phi and theta, number of plot points in both phi and theta, and the color
bumpySphere :: Float  -- the mean radius of the bumpy sphere
  -> Float  -- the relative amplitude of the bumpy sphere e.g. if this is set to 0.2, then the radius will range from 0.8 to 1.2 times the mean radius
  -> Float  -- the m value is the number of wobbles in the phi direction
  -> Float  -- the n value is the number of wobbles in the theta direction
  -> Int  -- the number of phi values for plot points
  -> Int  -- the number of theta values for plot points
  -> Int  -- the color of the bumpy sphere
  -> Plist
bumpySphere meanRadius relativeAmplitude mVal nVal numPhis numThetas color = nub $ zipWith (\(x, y, z) c -> (round x, round y, round z, c)) xyzs (repeat color)  -- This takes all the plot points in (x, y, z) and tuples them up with the color of the bumpy sphere.
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
                     where x = rho * cos (theta) * sin (phi)  -- This is the x component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                           y = rho * sin (theta) * sin (phi)  -- This is the y component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).
                           z = rho * cos (phi)  -- This is the z component of the equations for converting spherical co-ordinates (rho, phi, theta) to cartesian (x, y, z).


-- scalar field 2D to U list takes a 2D array of scalar values and turns them into a list of U sets representing a plot filling a level plane in Minecraft within certain x and z bounds at some height and the U value for each is just the scalar value unchanged.
sf2DtoUList :: ScalarField2D -> Ulist
sf2DtoUList inpSF = [(zi, 100, xi, uVal zi xi) | zi <- zVals, xi <- xVals]  -- For each x value and for each z value, the U set has those x and z values, height of 100, and U value corresponding to using that x and z to index a row and column of the input.
   where dim2 = length inpSF  -- The second dimension size is the number of points in the second dimension of the 2D array.
         dim1 = length $ inpSF !! 0  -- The first dimension size is the number of points in the first dimension of the 2D array.  This assumes all dim1s (of all dim1-rows) will be equal to the size of the first.
         xVals = [1 .. dim2]  -- The list of x values ranges from 1 to the second dimension size.
         zVals = [1 .. dim1]  -- The list of z values ranges from 1 to the first dimension size.
         uVal zi xi = (inpSF !! pred zi) !! pred xi  -- The U value for the result is ripped straight from the input.


-- scalar field 3D to U list takes a 3D array of scalar values and turns them into a list of U sets representing a 3D plot of points, a 3D grid of dots with spaces between, where each dot is a minecraft block or clump of blocks with color representing field magnitude at each location.  The U value for each is just the scalar value unchanged.
sf3DtoUList :: ScalarField3D  -- A 3D array of scalar values
  -> Int  -- clump size is how many blocks to a clump e.g. set it to 1 for each plot point to be one block, set to 2 for each plot to be a clump of 8 blocks (2 by 2 by 2)
  -> Int  -- space size is how many blocks are between successive clumps
  -> Ulist
sf3DtoUList inpSF clumpSize spaceSize = [(mc_x eq_y_ind dy, mc_y eq_z_ind dz, mc_z eq_x_ind dx, uVal eq_y_ind eq_z_ind eq_x_ind) | eq_x_ind <- eq_x_inds, eq_y_ind <- eq_y_inds, eq_z_ind <- eq_z_inds, dx <- dxs, dy <- dys, dz <- dzs]  -- The resultant U list takes care of every index in x, y, and z, and every dx, dy, dz with a block for each.  There is a conversion between Minecraft (x, y, z) and equation (x, y, z).
   where pitch = clumpSize + spaceSize  -- The pitch of the pattern is the sum of how big a block is and how far the space between clumps is.
         dim3 = length inpSF  -- The third dimension size is the number of points in the third dimension of the 2D array.
         dim2 = length $  inpSF !! 0  -- The second dimension size is the number of points in the second dimension of the 2D array.  This assumes all dim2s (of all dim2-rows) will be equal to the size of the first.
         dim1 = length $ (inpSF !! 0) !! 0  -- The first dimension size is the number of points in the first dimension of the 2D array.  This assumes all dim1s (of all dim1-rows) will be equal to the size of the first.
         eq_z_inds = [1 .. dim3]  -- The list of z indices ranges from 1 to the third dimension size.
         eq_y_inds = [1 .. dim2]  -- The list of y indices ranges from 1 to the second dimension size.
         eq_x_inds = [1 .. dim1]  -- The list of x indices ranges from 1 to the first dimension size.
         [dxs, dys, dzs] = replicate 3 [1 .. clumpSize]  -- dxs is a list of offsets for the blocks within a clump in the x direction, similar dys and y direction, dzs and z direction.
         mc_x eq_y_ind dy = (pred eq_y_ind) * pitch + floor (0.5 * fromIntegral pitch) + dy  -- The Minecraft x location of a block is based on the product of the equation y index multiplied by the plot's pitch, with an adjustment for alignment, plus a dy to locate the block within the clump.
         mc_y eq_z_ind dz = (pred eq_z_ind) * pitch + floor (0.5 * fromIntegral pitch) + dz  -- The Minecraft y location of a block is based on the product of the equation z index multiplied by the plot's pitch, with an adjustment for alignment, plus a dz to locate the block within the clump.
         mc_z eq_x_ind dx = (pred eq_x_ind) * pitch + floor (0.5 * fromIntegral pitch) + dx  -- The Minecraft z location of a block is based on the product of the equation x index multiplied by the plot's pitch, with an adjustment for alignment, plus a dx to locate the block within the clump.
         uVal xi yi zi = ((inpSF !! pred zi) !! pred yi) !! pred xi  -- The U value for the result is ripped straight from the input.


-- scale U list linear converts a U list to a P list, which means to turn magnitudes into colors.  Linear means each block color represents the same range size of magnitude values (doesn't try for equal numbers of block colors).  You can specify some number of the lowest and highest values to be given coldest and hottest colors (respectively) and be excluded from the magnitude calculations.
scaleUListLinear :: Ulist  -- the input U list
  -> Int  -- the rainbow number (1 or 2)
  -> Int  -- the number of low values to assign the min color and exclude from scaling calculations
  -> Int  -- the number of high values to assign the max color and exclude from scaling calculations
  -> Plist
scaleUListLinear inpList rainbNum numLowDrops numHighDrops = concat [lowPList, midPList, highPList]  -- The resultant list has the low values at color 1, the middling values with colors ranging from 1 to max, and the high values with color max.
   where sortedInpList = sortBy (\(_, _, _, u1) (_, _, _, u2) -> compare u1 u2) inpList  -- This takes the input U list and sorts the entries in terms of low to high magnitudes.
         midInpList = drop numLowDrops $ reverse $ drop numHighDrops $ reverse $ sortedInpList  -- This takes the sorted input list and takes some number of entries of the high and low ends, based on the values from the input.
         midMinMag = minimum $ map (\(_, _, _, m) -> m) midInpList  -- This returns the smallest value from the list that's been sorted and trimmed.
         midMaxMag = maximum $ map (\(_, _, _, m) -> m) midInpList  -- This returns the biggest value from the list that's been sorted and trimmed.
         midMagRange = midMaxMag - midMinMag  -- This is the difference between the biggest and smallest values from the list that's been sorted and trimmed.
         lowInps = take numLowDrops $ sortedInpList  -- This is the list of low inputs that were trimmed of the original list.
         highInps = reverse $ take numHighDrops $ reverse $ sortedInpList  -- This is the list of high inputs that were trimmed of the original list.
         lowPList = map (\(x, y, z, u) -> (x, y, z, 1)) lowInps  -- This takes the low trimmed inputs and gives them all color 1.
         highPList = map (\(x, y, z, u) -> (x, y, z, numColors)) highInps  -- This takes the high trimmed inputs and gives them all the highest color number.
         numColors = case rainbNum of  -- Rainbow number 1 has exactly 11 colors and rainbow number 2 has exactly 22 colors.
                        1 -> 11
                        2 -> 22
                        _ -> undefined
         midPList = map uSetToPSet midInpList  -- This maps the function for converting a U value to a color to the trimmed sorted input list.
            where uSetToPSet (ux, uy, uz, uVal) = (ux, uy, uz, colorUVal uVal)  -- Changing a U set to a P set is to apply a colorizing function to the U value and leave the co-ordinates the same.
                  colorUVal uVal = min numColors $ max 1 $ {- The part of the line before this is a couple of safety checks for underflowing or overflowing the number of colors in the rainbow -} floor $ (fromIntegral numColors + 0.0001) * relativeUVal uVal  -- This maps a relative U value (between 0 and 1) to a position along the rainbow.
                  relativeUVal uVal = (uVal - midMinMag) / midMagRange  -- This returns a number that should be normalized to the range of 0 to 1 and represents a given u value's relative size within the trimmed sorted input list.


{-
scaleulistlinearwheight :: Ulist -> Int -> Int -> Int -> Int -> Plist
scaleulistlinearwheight inpList rainbnum yMax numlowdrops numhighdrops = concat [lowplist, subplist, highplist]
   where subinpList = drop numlowdrops $ reverse $ drop numhighdrops $ reverse $ sort inpList
         subminmag = minimum $ map (\(_, _, _, m) -> m) subinpList
         submaxmag = maximum $ map (\(_, _, _, m) -> m) subinpList
         submagrange = submaxmag - subminmag
         subplist = map uvaltopval subinpList
         lowinps = take numlowdrops $ sort inpList
         highinps = reverse $ take numhighdrops $ reverse $ sort inpList
         lowplist = map (\(x, y, z, u) -> (x, 1, z, 1)) lowinps
         highplist = map (\(x, y, z, u) -> (x, yMax, z, numcolors)) highinps
         coloruval uval = floor $ (fromIntegral numcolors + 0.0001) * (uval - subminmag) / submagrange
         numcolors = case rainbnum of 
                        1 -> 11
                        2 -> 22
                        _ -> undefined
         uvaltopval (ux, uy, uz, mag) = (ux, pyval, uz, coloruval mag)
            where pyval = floor $ (fromIntegral yMax + 0.0001) * (mag - subminmag) / submagrange
-}


-- scale U list equal colors converts a U list to a P list, which means to turn magnitudes into colors.  This type of scaling assigns equals numbers of each block color (doesn't try for equal range sizes for each color).
scaleUListEqualColors :: Ulist  -- the input U list
  -> Int  -- the rainbow number (1 or 2)
  -> Plist
scaleUListEqualColors inpList rainbNum = zipWith (\(x, y, z, _) c -> (x, y, z, c)) sortedInpList colorList  -- The result zips the co-ordinate list sorted by magnitude with the sorted color list.
   where sortedInpList = sortBy (\(_, _, _, u1) (_, _, _, u2) -> compare u1 u2) inpList  -- This takes the input U list and sorts the entries in terms of low to high magnitudes.
         numColors = fromIntegral (length inpList) / case rainbNum of  -- Rainbow number 1 has exactly 11 colors and rainbow number 2 has exactly 22 colors.
                                                           1 -> 11.0
                                                           2 -> 22.0
                                                           _ -> undefined
         changeInds = map ceiling [numColors, 2 * numColors ..]  -- This returns a list of indices where the color changes should occur and takes care of the rounding to whole numbers.
         numPerColor = map (\a -> (changeInds !! a) - (changeInds !! pred a)) [1 ..]  -- This returns a list of how many blocks of each color there are.  e.g. if there were 9 colors and 21 blocks, the list would be something like [2, 2, 3, 2, 2, 3, 2, 2, 3], to disperse which numbers are rounded up and which ones are rounded down.
         colorNumSubList colorNum = replicate (numPerColor !! pred colorNum) colorNum  -- This returns a list with a color repeated a number of times equal to the number of blocks that should be that color.
         colorList = concatMap colorNumSubList [1 ..]  -- This returns a list with each color repeated the number of times equal to the number of blocks that should be that color.  e.g. if there were 9 colors and 21 blocks, the list would be something like [1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6, 7, 7, 8, 8, 9, 9, 9].


-- mono color U list converts a U list to a P list, which means to turn magnitudes into colors.  This one is used when you don't want to scale U values to colors and just want to color all the blocks the same color.
monoColorUList :: Ulist -> Int -> Plist
monoColorUList inpList color = zipWith (\(x, y, z, _) c -> (x, y, z, c)) inpList (repeat color)


-- This moves all the blocks of a plot some number of grid spaces in the up (+y) direction.
movePListUp :: Plist -> Int -> Plist
movePListUp inpPList moveDist    = map (\(x, y, z, c) -> (x, y + moveDist, z, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the down (-y) direction.
movePListDown :: Plist -> Int -> Plist
movePListDown inpPList moveDist  = map (\(x, y, z, c) -> (x, y - moveDist, z, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the North (-z) direction.
movePListNorth :: Plist -> Int -> Plist
movePListNorth inpPList moveDist = map (\(x, y, z, c) -> (x, y, z - moveDist, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the East (+x) direction.
movePListEast :: Plist -> Int -> Plist
movePListEast inpPList moveDist  = map (\(x, y, z, c) -> (x + moveDist, y, z, c)) inpPList

-- This moves all the blocks of a plot some number of grid spaces in the South (+z) direction.
movePListSouth :: Plist -> Int -> Plist
movePListSouth inpPList moveDist = map (\(x, y, z, c) -> (x, y, z + moveDist, c)) inpPList

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


-- thicken takes a Plist and a number and thickens all parts of that plot by that amount.
thicken :: Plist -> Int -> Plist
thicken inpPList thickness = nub $ concatMap makeMore inpPList  -- The result is obtained by taking the input list, listing all points around all input list points to some (x, y, z) distances, flattening that list of lists to a list, and removing redundantly equivalent values.
   where offset = floor (fromIntegral thickness / 2.0)  -- The offset is half the thickness, used in the equations for resultant co-ordinates to make sure the thickening is centered around the input blocks.
         [dxs, dys, dzs] = replicate 3 ([1 .. thickness])  -- The list of dx values is a list of numbers from 1 to the thickness value from the input.  Likewise dy and dz.
         makeMore (inp_x, inp_y, inp_z, inpC) = [(inp_x + dx - offset, inp_y + dy - offset, inp_z + dz - offset, inpC) | dx <- dxs, dy <- dys, dz <- dzs]  -- The list of blocks around a block is a block for each combination of dx and dy and dz, using offset to keep the output centered.


-- opacity takes a Plist and a number and randomly removes that percentage of blocks using seeded random number generation.
opacity :: Plist -> Int -> Plist
opacity inpPList opacity = map fst $ filter (\(_,b) -> b == True) (zip inpPList keepThrowList)  -- The result is obtained by taking some list of true and false values and keeping/removing input list values according to it.
   where numberifyBlock (inp_x, inp_y, inp_z, _) = inp_x * 1000000 + inp_y * 10000 + inp_z  -- This equation takes the x, y, and z values of a block and turns them into a single number for seeding the RNG.
         numberifiedList = map numberifyBlock inpPList  -- This is a list of RNG seeds corresponding to the list of input block co-ordinates.
         diceRollList = map (\a -> fst (randomR (1::Int,100) (mkStdGen a))) numberifiedList  -- This takes the list of RNG seeds and converts it into a list of values with each being the result of a 100-sided dice roll.
         keepThrowList = map (\a -> if opacity > a then True else False) diceRollList  -- This takes the list of 100-sided dice rolls, uses the opacity value from the input, and makes a list of True/False values for which of the input blocks to keep and which to filter out.


-- py string a block takes a P set and a rainbow number and returns the two lines of MCEdit python for plotting a block of some type at some location.  It might be colored wool (with a color data value) or some other block type (without an additional data value).
pyStringABlock :: Pset -> Int -> String
pyStringABlock (inp_x, inp_y, inp_z, inpC) rainbNum = case rainbNum of  -- This calls the function for turning a block's P set into MCEdit python, whatever the rainbow number.
                                                      1 -> rainb1Str
                                                      2 -> rainb2Str
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


-- py string a plot takes a whole plot and turns it into a string.  For any graph that isn't tiny, this will generate a long .py file.  It will have a line at the top and then a line or two for each block.
pyStringAPlot :: Plist -> Int -> String
pyStringAPlot inpPList rainbNum = "def perform(level, box, options):\n" ++ concatMap (\x -> pyStringABlock x rainbNum) inpPList


-- py strings a plot takes a whole plot and turns it into either one or more strings, each resultant string being the contents of an output py.  If the plot is small, the result of this will be one string for one output py.  If the plot is big, the result will be some bigger number of strings, one string for one output py each.  The maximum for each output py is to take care of 20000 plot points, so if you give this a plot of 50123 blocks, there will be a list of 3 outputs.
pyStringsAPlot :: Plist  -- the P list for some plot
  -> Int  -- the rainbow number (either 1 or 2)
  -> Bool  -- a bool for whether to crop blocks above the Minecraft ceiling
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


-- write ops take some name, and some list of strings, each string for an output py, and generates a list of IO operations where each operation is to write a py file with a sequential name made of the input name plus a sequential number.  e.g. if given "foo" and a list of 3 strings, it will write foo1.py consisting of the first string, foo2.py consisting of the second, foo3.py consisting of the third.
writeOps :: String -> [String] -> [IO ()]
writeOps seriesName pyStrList = map oneOp inds  -- The result is got by mapping the function that calls writeFile with the list of output indices.
   where inds = [1,2..length pyStrList]  -- The list of output indices ranges from 1 to the number of strings from the input.
         fileNames = map (\x -> seriesName ++ (show x) ++ ".py") inds  -- The file names are sequential with the input name, a number, and extension .py
         oneOp ind = writeFile (fileNames !! pred ind) (pyStrList !! pred ind)  -- One output operation matches one sequential name with one string and writes one file.




--------------------------------------------------
--------   - Results and output stuff -   --------
--------------------------------------------------

sc2Dfield1 :: ScalarField2D
sc2Dfield1 = fill2DSF sfEqa2Df1 60 60 (-6) 6 (-6) 6

sc2Dfield2 :: ScalarField2D
sc2Dfield2 = fill2DSF sfEqa2Df2 60 60 (-6) 6 (-6) 6

sc2Dfield3 :: ScalarField2D
sc2Dfield3 = fill2DSF sfEqa2Df3 60 60 (-6) 6 (-6) 6

sc2Dfield4 :: ScalarField2D
sc2Dfield4 = fill2DSF sfEqa2Df4 60 60 (-6) 6 (-6) 6

sc2Dfield5 :: ScalarField2D
sc2Dfield5 = add2DSF sc2Dfield3 sc2Dfield4

sc2Dfield6 :: ScalarField2D
sc2Dfield6 = add2DSF sc2Dfield3 (map (map negate) sc2Dfield4)


sc3Dfield1 :: ScalarField3D
sc3Dfield1 = fill3DSF sfEqa3Df1 20 20 20 (-5) 5 (-5) 5 (-5) 5 -- n, m, o indices d 0.5 units of equation x y z.

sc3Dfield2 :: ScalarField3D
sc3Dfield2 = fill3DSF sfEqa3Df2 20 20 20 (-5) 5 (-5) 5 (-5) 5 -- n, m, o indices d 0.5 units of equation x y z.

sc3Dfield3 :: ScalarField3D
sc3Dfield3 = fill3DSF sfEqa3Df3 20 20 20 (-5) 5 (-5) 5 (-5) 5 -- n, m, o indices d 0.5 units of equation x y z.

sc3Dfield4 :: ScalarField3D
sc3Dfield4 = fill3DSF sfEqa3Df4 20 20 20 (-5) 5 (-5) 5 (-5) 5 -- n, m, o indices d 0.5 units of equation x y z.

sc3Dfield5 :: ScalarField3D
sc3Dfield5 = addsc3Dfields sc3Dfield3 sc3Dfield4     -- n, m, o indices d 0.5 units of equation x y z.

sc3Dfield6 :: ScalarField3D
sc3Dfield6 = addsc3Dfields sc3Dfield3 (map (map (map negate)) sc3Dfield4) -- n, m, o indices d 0.5 units of equation x y z.


-- World 01 is 6 medium-size plots of 2D scalar fields of monopoles and dipoles.
go_w01 = sequence [sequence writeOps_w01_1, sequence writeOps_w01_2, sequence writeOps_w01_3, sequence writeOps_w01_4, sequence writeOps_w01_5, sequence writeOps_w01_6]

writeOps_w01_1 = writeOps "Plot-w01-1-" $ pyStringsAPlot (movePListWest (scaleUListEqualColors (sf2DtoUList sc2Dfield1) 1)  0) 1 True

writeOps_w01_2 = writeOps "Plot-w01-2-" $ pyStringsAPlot (movePListWest (scaleUListEqualColors (sf2DtoUList sc2Dfield2) 1) 70) 1 True

writeOps_w01_3 = writeOps "Plot-w01-3-" $ pyStringsAPlot (movePListNorth (movePListWest (scaleUListEqualColors (sf2DtoUList sc2Dfield3) 1) 0)   70) 1 True

writeOps_w01_4 = writeOps "Plot-w01-4-" $ pyStringsAPlot (movePListNorth (movePListWest (scaleUListEqualColors (sf2DtoUList sc2Dfield4) 1) 70)  70) 1 True

writeOps_w01_5 = writeOps "Plot-w01-5-" $ pyStringsAPlot (movePListNorth (movePListWest (scaleUListEqualColors (sf2DtoUList sc2Dfield5) 1) 0)  140) 1 True

writeOps_w01_6 = writeOps "Plot-w01-6-" $ pyStringsAPlot (movePListNorth (movePListWest (scaleUListEqualColors (sf2DtoUList sc2Dfield6) 1) 70) 140) 1 True


-- World 02 is a large plot of the 2D scalar field of a monopole.
go_w02 = sequence writeOps_w02

writeOps_w02 = writeOps "Plot-w02-" $ pyStringsAPlot (scaleUListEqualColors (sf2DtoUList (fill2DSF sfEqa2Df1 200 200 (-6) 6 (-6) 6)) 2) 2 True


-- World 03 is a large plot of the 2D scalar field of a dipole, plus some field lines and equipotential curves.
go_w03 = sequence [sequence writeOps_w03a, sequence writeOps_w03b, sequence writeOps_w03c, sequence writeOps_w03d]

writeOps_w03a = writeOps "Plot-w03a-" $ pyStringsAPlot (scaleUListEqualColors (sf2DtoUList (fill2DSF sfEqa2Df6 200 200 (-6) 6 (-6) 6)) 2) 2 True

writeOps_w03b = writeOps "Plot-w03b-" $ pyStringsAPlot eqpcplist 1 True
   where eqpcplist = eqPoCrvs2D sfEqa2Df6 [(0.75, 0.04, 11), (-0.75, 0.04, 2), (0.25, 0.025, 11), (-0.25, 0.025, 3)] 200 200 (-6) 6 (-6) 6

writeOps_w03c = writeOps "Plot-w03c-" $ pyStringsAPlot flplist 1 True
   where flplist = fieldLines2D grad2DSF6 startfieldpointscolors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 (\(xi, yi, mi) -> mi < 30 && abs xi < 9) 1.0 0.1 False
            where startfieldpointscolors1 = zip startfieldpoints1 randcolors
                     where startfieldpoints1 = map (\(x, y) -> (x - 3, y)) (thetaArray 0.2 12)

writeOps_w03d = writeOps "Plot-w03d-" $ pyStringsAPlot flplist 1 True
   where flplist = fieldLines2D grad2DSF6 startfieldpointscolors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 (\(xi, yi, mi) -> mi < 30 && abs xi < 9) 1.0 0.1 True
            where startfieldpointscolors1 = zip startfieldpoints1 randcolors
                     where startfieldpoints1 = map (\(x, y) -> (x + 3, y)) (thetaArray 0.2 12)


-- World 04 is a large plot of the 2D scalar field of a homo-dipole, plus some field lines and equipotential curves.
go_w04 = sequence [sequence writeOps_w04a, sequence writeOps_w04b, sequence writeOps_w04c, sequence writeOps_w04d]

writeOps_w04a = writeOps "Plot-w04a-" $ pyStringsAPlot (scaleUListEqualColors (sf2DtoUList (fill2DSF sfEqa2Df5 200 200 (-6) 6 (-6) 6)) 2) 2 True

writeOps_w04b = writeOps "Plot-w04b-" $ pyStringsAPlot eqpcplist 1 True
   where eqpcplist = eqPoCrvs2D sfEqa2Df5 [(1.143, 0.04, 11), (0.757, 0.03, 7), (0.600, 0.02, 3)] 200 200 (-6) 6 (-6) 6

writeOps_w04c = writeOps "Plot-w04c-" $ pyStringsAPlot flplist 1 True
   where flplist = fieldLines2D grad2DSF5 startfieldpointscolors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 (\(xi, yi, mi) -> abs xi < 9 && abs yi < 9) 1.0 0.1 True
            where startfieldpointscolors1 = zip startfieldpoints1 randcolors
                     where startfieldpoints1 = map (\(x, y) -> (x - 3, y)) (thetaArray 0.2 12)

writeOps_w04d = writeOps "Plot-w04d-" $ pyStringsAPlot flplist 1 True
   where flplist = fieldLines2D grad2DSF5 startfieldpointscolors1 (50.0 / 3.0) (50.0 / 3.0) 6.0 6.0 (\(xi, yi, mi) -> abs xi < 9 && abs yi < 9) 1.0 0.1 True
            where startfieldpointscolors1 = zip startfieldpoints1 randcolors
                     where startfieldpoints1 = map (\(x, y) -> (x + 3, y)) (thetaArray 0.2 12)


go_w05 = sequence writeOps_w05

writeOps_w05 = writeOps "Plot-w05-" $ pyStringsAPlot (movePListUp (bumpySphere 80 0.2 4 8 600 600 11) 127) 1 True


main = go_w05    -- temporary goer
