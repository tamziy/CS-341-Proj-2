// Author: Tam Le
// UIN: 674428071
// File: library.fs
// Project: Image Processing in F#
// Description:
// library.fs contains core logic for image operations.
// Manipulates images that are in ppm format.

namespace ImageLibrary

module Operations =
 
// Helper function to convert an RGB tuple to Grayscale
// Helper function to convert an RGB tuple to Grayscale
  let toGray (r, g, b) =
      let gray = int (float r * 0.299 + float g * 0.587 + float b * 0.114)
      (gray, gray, gray)


  // Function that converts the entire image to Grayscale
  //Uses helper function 'toGray'
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    let grayImage = List.map (fun row -> List.map toGray row) image
    grayImage



  
  // Helper function to apply thresholding to a single RGB tuple
// Helper function to apply thresholding to a single RGB tuple
  let applyThreshold threshold (r, g, b) =
      let thres r = if r > threshold then 255 else 0
      (thres r, thres g, thres b)

  // Function that applies thresholding to the entire image
  // uses helper function 'applyThreshold'
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
      let thresholdedImage = List.map (fun row -> List.map (applyThreshold threshold) row) image
      thresholdedImage


  //Helper function for 'FlipHorizontal' Function
  //Takes a single row as arguement and returns the row reversed using 'List.rev'
  let reverseRow row = 
    List.rev row

  //Function that flips the image horizontally by reverse each individual row using
  //helper function 'reverseRow'.
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    let flippedImage = List.map reverseRow image
    flippedImage


  // Helper function for calculating the distance between two pixels using the Pythagorean theorem
  let colorDistance (r1, g1, b1) (r2, g2, b2) =
      sqrt((float (r1 - r2) ** 2.0) + (float (g1 - g2) ** 2.0) + (float (b1 - b2) ** 2.0))

  // Helper function to check edge
  let isEdge threshold pixel rightPixel bottomPixel =
      let d1 = colorDistance pixel rightPixel
      let d2 = colorDistance pixel bottomPixel
      d1 > float threshold || d2 > float threshold

    // EdgeDetect function to detect edge of images
  let EdgeDetect (width:int)
                 (height:int)
                 (depth:int)
                 (image:(int*int*int) list list)
                 (threshold:int) =
      List.init (height - 1) (fun i -> 
          let row = List.item i image
          List.init (width - 1) (fun j -> 
              let pixel = List.item j row
              let rightPixel = List.item (j + 1) row
              let bottomPixel = List.item j (List.item (i + 1) image)
              if isEdge threshold pixel rightPixel bottomPixel then (0, 0, 0) else (255, 255, 255)
          )
      )




// Helper Function that fetches a column from a 2D List
  let getColumn col image =
    List.map (fun row -> List.item col row) image

//Helper Function that turns rows into columns and vice versa
//Uses 'getColumn' helper function to fetch columns.
  let switchColumnRow image =
    let rowCount = List.length image
    let colCount = List.length (List.head image)

    List.map (fun col -> getColumn col image) [0 .. colCount - 1]


//Function that rotates a ppm image 90 degrees to the right
//Uses helper functions 'getColumn' and 'switchColumnRow'
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    let switchedCRimage = switchColumnRow image
    let rotated90RightImage = List.map reverseRow switchedCRimage
    rotated90RightImage

