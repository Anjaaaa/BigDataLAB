# the way you visualize the images of digit

colors <- c('white','black'); cus_col <- colorRampPalette(colors=colors)

z <- matrix(as.numeric(data_train[10,257:2]),16,16,byrow=T)[,16:1]

image(t(z),col=cus_col(256))

