add_stroke_to_shape <- function(image, color = "white", size = 2) {
  # 1. Extract the alpha channel (the shape)
  mask <- image_channel(image, "alpha")
  
  # 2. Grow the shape (dilate) to create the stroke area
  # kernel size determines stroke width
  kernel <- paste0("Disk:", size) 
  background <- image_morphology(mask, "Dilate", kernel)
  
  # 3. Color the background stroke
  background <- image_colorize(background, 100, color)
  
  # 4. Paste the original image on top of the stroke
  image_composite(background, image, operator = "Over")
}

#if we are moving to transparent images
