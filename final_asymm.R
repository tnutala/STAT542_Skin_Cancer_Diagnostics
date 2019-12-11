#WIP

library("imager")

circlemask = draw_circle(imfill(256, 256, val = c(0, 0, 0)), 128, 128, 128)
angleinc = 5
angles = 0:(90 / angleinc) * angleinc

preprocess = function(imgpath) {
  img = load.image(imgpath)
  img.top = imsub(img, y <= 4)

  r = mean(as.numeric(channel(img.top, 1)))
  g = mean(as.numeric(channel(img.top, 2)))
  b = mean(as.numeric(channel(img.top, 3)))
  
  img.pad = pad(img, dim(img)[1] - dim(img)[2], "y", 0, c(r, g, b))
  img.res = resize(img.pad, 256, 256)
  img.masked = mult(imlist(img.res, circlemask))
}

checkasymmetry = function(img) {
  img.left = imsub(img, x <= 128)
  img.left.mirror = mirror(img.left, "x")
  img.right = imsub(img, x > 128)
  
  img.left.r = channel(img.left.mirror, 1)
  img.right.r = channel(img.right, 1)
  img.left.g = channel(img.left.mirror, 2)
  img.right.g = channel(img.right, 2)
  img.left.b = channel(img.left.mirror, 3)
  img.right.b = channel(img.right, 3)
  
  
  diffs = (as.numeric(img.left.r) - as.numeric(img.right.r))^2 + 
    (as.numeric(img.left.g) - as.numeric(img.right.g))^2 +
    (as.numeric(img.left.b) - as.numeric(img.right.b))^2
  
  sqrt(mean(diffs))
}

checkasymmetry.rot = function(rot, img) {
  img.rot = imrotate(img, rot)
  img.crop = crop.borders(img.rot, (dim(img.rot)[1] - 256)/2, (dim(img.rot)[2] - 256)/2)
  checkasymmetry(img.crop)
}

checkangles = function(img) {
  min(sapply(angles, function(a) {
    checkasymmetry.rot(a, img)
  }))
  
}

measureasymm = function(imgpath) {
  print(paste("Measuring", imgpath))
  img = preprocess(imgpath)
  checkangles(img)
}

measureasymm.dir = function(d) {
  sapply(list.files(d), function(x) {
    measureasymm(file.path(d, x))
  })
}

