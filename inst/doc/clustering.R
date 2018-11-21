## ------------------------------------------------------------------------
head(USArrests, 3) 

## ------------------------------------------------------------------------
d = dist(USArrests, method = "euclidean") 

## ------------------------------------------------------------------------
fit = hclust(d, method="ward.D2") 

## ----cluster_fig,fig.width=5, fig.height=5, echo=1-----------------------
plot(fit, labels=rownames(d)) 
groups = cutree(fit, k=3) 
rect.hclust(fit, k=3, border="rosybrown") 

## ----cluster_fig, eval=FALSE, echo=2:3-----------------------------------
#  plot(fit, labels=rownames(d))
#  groups = cutree(fit, k=3)
#  rect.hclust(fit, k=3, border="rosybrown")

## ----eval=FALSE----------------------------------------------------------
#  kmeans(USArrests, centers=3)

## ------------------------------------------------------------------------
apply(USArrests,2,mean) 

## ------------------------------------------------------------------------
std_usa = scale(USArrests) 

## ----eval=FALSE----------------------------------------------------------
#  kmeans(std_usa, centers=3)

## ----eval = FALSE--------------------------------------------------------
#  plot(hclust(dist(std_usa)))

