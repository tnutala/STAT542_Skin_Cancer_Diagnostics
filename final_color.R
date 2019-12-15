library("imager")

intensity = function(img) {
  as.numeric(grayscale(img))
}

measureintensity = function(imgpath) {
  gc()
  print(imgpath)
  img = load.image(imgpath)
  mean(intensity(img))
}

img = load.image(file.path("benign", list.files("benign")[1]))
measureintensity.dir = function(d) {
  sapply(list.files(d), function(x) {
    measureintensity(file.path(d, x))
  })
}

measureintensityvar = function(imgpath) {
  gc()
  print(imgpath)
  img = load.image(imgpath)
  var(intensity(img))
}

measureintensityvar.dir = function(d) {
  sapply(list.files(d), function(x) {
    measureintensityvar(file.path(d, x))
  })
}

blue = function(img) {
  b = channel(img, 3)
  rg = pmax(as.numeric(channel(img, 1)), as.numeric(channel(img, 2)))
  pmax(0, b - rg)
}

measureblue = function(imgpath) {
  gc()
  print(imgpath)
  img = load.image(imgpath)
  mean(blue(img))
}

measurebluevar = function(imgpath) {
  gc()
  print(imgpath)
  img = load.image(imgpath)
  var(blue(img))
}

measureblue.dir = function(d) {
  sapply(list.files(d), function(x) {
    measureblue(file.path(d, x))
  })
}

measurebluevar.dir = function(d) {
  sapply(list.files(d), function(x) {
    measureblue(file.path(d, x))
  })
}

red = function(img) {
  r = channel(img, 1)
  gb = pmax(as.numeric(channel(img, 2)), as.numeric(channel(img, 3)))
  pmax(0, r - gb)
}

measurered = function(imgpath) {
  gc()
  print(imgpath)
  img = load.image(imgpath)
  mean(red(img))
}

measureredvar = function(imgpath) {
  gc()
  print(imgpath)
  img = load.image(imgpath)
  var(red(img))
}

measurered.dir = function(d) {
  sapply(list.files(d), function(x) {
    measurered(file.path(d, x))
  })
}

measureredvar.dir = function(d) {
  sapply(list.files(d), function(x) {
    measureredvar(file.path(d, x))
  })
}