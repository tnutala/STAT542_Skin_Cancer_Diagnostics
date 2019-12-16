library(opencv)

measureedge = function(imgpath) {
  img = ocv_read(imgpath)
  mean(as.numeric(ocv_bitmap(ocv_edges(ocv_resize(img, 256, 256)))))
}

measureedge.dir = function(d) {
  sapply(list.files(d), function(x) {
    measureedge(file.path(d, x))
  })
}

