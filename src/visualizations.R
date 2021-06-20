fit = cmdscale(dmatrix, eig = TRUE)
x= fit$points[,1]
y= fit$points[,2]
z = landscape$fitness



plot(sort(z))


scatterplot3d(x,y,z, angle = 70, color = 'blue')


plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(dmatrix), cex=.7) 
