
#install library.....

install.packages("corrplot")
install.packages("ggplot2")
install.packages("RColorBrewer" )
install.packages("sp")
install.packages("rgdal")
install.packages("leaflet")
install.packages("automap")
install.packages("lattice")
install.packages("raster")
install.packages("gstat")
install.packages("grid")
install.packages("data.table")


#read the data....
library(data.table)
Earthquake=read.csv("C:\\r project\\data\\database.csv")

#run linear regression to find Magnitude~Depth realition...
fit= lm(Magnitude~Depth,data = Earthquake)
summary(fit)

"
Residuals:
  Min      1Q  Median      3Q     Max 
-0.4334 -0.2806 -0.1772  0.1225  3.2208 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.877e+00  3.191e-03 1841.45  < 2e-16 ***
  Depth       8.091e-05  2.254e-05    3.59 0.000331 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.423 on 23410 degrees of freedom
Multiple R-squared:  0.0005502,	Adjusted R-squared:  0.0005076 
F-statistic: 12.89 on 1 and 23410 DF,  p-value: 0.0003313


"
#we can see there is strong Significant, but very low influence.

plot(fit)

#run multi check correltion
library(corrplot)
mat = cor(Earthquake[, c(-1,-2,-5,-10,-17:-21)])
corrplot(mat, method = "number")

library(ggplot2)
ggplot(data = Earthquake, aes(x=Depth, y=Magnitude))+
  geom_point()+
  theme_minimal()

#show density and distribution of earthquak by deph an magnitude
#we can see 2 Points of interest 10 km and 35 km


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
