Earthquake=read.csv("D:\\data sience\\project\\R PROJECT NEW\\database.csv")


fit= lm(Magnitude~Depth,data = Earthquake)
summary(fit)

library(corrplot)

mat = cor(Earthquake[, c(-1,-2,-5,-10,-17:-21)])

corrplot(mat, method = "number")

library(ggplot2)
ggplot(data = Earthquake, aes(x=Depth, y=Magnitude))+
  geom_point()+
  theme_minimal()
library( RColorBrewer )

ggplot(data = Earthquake, aes(x=Depth, y=Magnitude))+
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  scale_fill_gradientn(
    colours = rev( brewer.pal( 7, "Spectral" ) )
  ) +
  theme_minimal()

library(sp)
library(rgdal)

coordinates(Earthquake)=~Longitude+Latitude # spatial data frame
plot(Earthquake)

# interctive map<--------
library(leaflet)

leaflet(Earthquake)%>% addTiles()%>% addCircles()
################

library(automap)
library(lattice)

fit = autoKrige(formula = Magnitude~ 1,input_data=Earthquake) # autokriging for the data
summary(fit)
plot(fit)
plot(fit$krige_output)

# create a good variogram
fit_var=autofitVariogram(formula = Magnitude~ 1,input_data=Earthquake)

dgt <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2

mdl <- fit_var$var_model
cls <- as.character(mdl[2, "model"])
ngt <- sum(mdl[1, "psill"])
sll <- sum(mdl[, "psill"])
rng <- sum(mdl[, "range"])
lbl <- paste("Model:", cls,
             "\nNugget:", round(ngt, dgt(ngt)),
             "\nSill:", round(sll, dgt(ngt)),
             "\nRange:", round(rng, dgt(rng)))

if (cls %in% c("Mat", "Ste")) {
  kpp <- mdl[2, "kappa"]
  lbl <- paste(lbl, "\nKappa:", dgt(kpp))
}

## create plot
xyplot(gamma ~ dist, data = fit_var$exp_var,
       main = "Experimental variogram and fitted variogram model", 
       xlab = "Distance", ylab = "Semi-variance",
       panel = function(x, y, ...) {
         gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
         ltext(max(x), 0.2 * max(y), lbl, font = 2, cex = .9, adj = c(1, 0), 
               col = "grey30")
       }, 
       # arguments required by gstat::vgm.panel.xyplot()
       labels = NULL, mode = "direct", model = mdl, 
       direction = c(fit_var$exp_var$dir.hor[1], fit_var$exp_var$dir.ver[1]))

library(raster)
library(gstat)

vgm <- variogram(Magnitude~Depth,Earthquake)
# choose the best model according to the smallest Nug
fitvgm <- fit.variogram(vgm, vgm(model="Nug"))

plot(vgm, model=fitvgm, as.table=TRUE)

# universal kriging
library(grid)
points <- coordinates(Earthquake)
points=SpatialPoints(points)


unKrig=autoKrige(Magnitude~Depth, Earthquake, new_data = Earthquake ,model="Sph")
plot(unKrig)
summary(unKrig)
