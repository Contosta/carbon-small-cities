##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
#This script analyzes patterns in above- and below-ground carbon stocks within Manchester, NH, USA
#as part of a paper submitted to Lanscape and Urban Planning (doi:##################)

#The datasets required to run this code are located in the carbon-small-cities repository and include
#"sites_scn.csv", "loc_sites.csv", "sitrcla.csv", "sitrloc.csv", and "sitr.csv" datasets

#Code was developed by A. Contosta

####################################################################################
####################################################################################
#Initial set up
####################################################################################
####################################################################################

#call libraries

library(nlme)
library(multcomp)

#run script to introduce multiple comparison methods for 'gls' objects.
model.matrix.gls <- function(object, ...) {
  model.matrix(terms(object), data = getData(object), ...)
}
model.frame.gls <- function(object, ...) {
  model.frame(formula(object), data = getData(object), ...)
}
terms.gls <- function(object, ...) {
  terms(model.frame(object), ...)
}

#set working directory and read in data
setwd("C:\\Users\\alix\\Box Sync\\UNH\\Projects\\CCS_Manchester\\Data\\R Projects\\carbon-small-cities\\Data")

#########################
#belowground carbon data#
#########################

#"sites_scn" = belowground soil carbon stocks by depth increment
#"loc_sites" = belowground soil carbon and nitrogen stocks across depth increments 
              #stratified by location within the yard as well as biogeochemical and socioeconomic characteristics of yards

sites_scn = read.table("sites_scn.csv", head = T, sep = ",")
loc_sites = read.table("loc_sites.csv", head = T, sep = ",")

#########################
#aboveground carbon data#
#########################

#sitrcla = aboveground biomass carbon stocks by FIA size class as well as 
           #biogeochemical and socioeconomic characteristics of yards
#sitrloc = aboveground biomass carbon stocks stratified by location within yard as well as
           #biogeochemical and socioeconomic characteristics of yards

sitrcla = read.table("sitrcla.csv", head = T, sep = ",")
sitrloc = read.table("sitrloc.csv", head = T, sep = ",")

####################################
#above- and belowground carbon data#
####################################

#sitr = total aboveground biomass carbon stocks, belowground soil carbon stocks across the soil profile, and 
        #biogeochemical and socioeconomic characteristics of yards

sitr = read.table("sitr.csv", head = T, sep = ",")

####################################################################################
####################################################################################
#Data preprocessing
####################################################################################
####################################################################################

#make new column in sitr for proportion of parcel comprised of yard (the managed part of the
#parcel)

sitr$peryd = sitr$yd / sitr$Area_Tot_Cor

#################################
#normality of response variables#
#################################

#belowground soil carbon across all depths
hist(sites_scn$stoC)

#belowground soil carbon within depth fractions
hist(sitr$stoC_at10)
hist(sitr$stoC_at20)
hist(sitr$stoC_at30)
hist(sitr$stoC_at40)
hist(sitr$stoC_at50)

#belowground soil carbon within depth fractions as a function of location in yard
hist(loc_sites$stoCloc_at10)
hist(loc_sites$stoCloc_at20)
hist(loc_sites$stoCloc_at30)
hist(loc_sites$stoCloc_at40)
hist(loc_sites$stoCloc_at50)

#aboveground biomass carbon across all yards
hist(sitr$Cseqha)

#aboveground biomass carbon as a function of size class
hist(sitrcla$Cseqha)

#aboveground biomass carbon as a function of location within yard
hist(sitrloc$Cseqha)

#belowground soil carbon data mostly follow normal distribution
#aboveground biomass carbon data do not.
#log transformation not possible due to presence of zero values

################################
#Outliers in response variables#
################################

#belowground soil carbon, all depths
boxplot(sites_scn$stoC)

#belowground soil carbon, within depths
boxplot(sitr$stoC_at10)
boxplot(sitr$stoC_at20)
boxplot(sitr$stoC_at30)
boxplot(sitr$stoC_at40)
boxplot(sitr$stoC_at50)

#belowground soil carbon, within depths and separated by yard location
boxplot(loc_sites$stoCloc_at10)
boxplot(loc_sites$stoCloc_at20)
boxplot(loc_sites$stoCloc_at30)
boxplot(loc_sites$stoCloc_at40)
boxplot(loc_sites$stoCloc_at50)

#aboveground biomass carbon, all size classes
boxplot(sitr$Cseqha)

#aboveground biomass carbon, within size classes
boxplot(sitrcla[sitrcla$fiac == "POL", ]$Cseqha)
boxplot(sitrcla[sitrcla$fiac == "SAP", ]$Cseqha)
boxplot(sitrcla[sitrcla$fiac == "SAW", ]$Cseqha)

#aboveground biomass carbon, all size classes, separated by yard location
boxplot(sitrloc$Cseqha)

#two of the yards appear to be outliers and have crazy high aboveground biomass values that may
#need to be removed

#################################
#Outliers in predictor variables#
#################################

#population density
boxplot(sitr$POP_Densit, ylab = "Population Density")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$POP_Densit,  xlab = "Population Density",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$POP_Densit,  xlab = "Population Density",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)


#housing age
boxplot(sitr$Hage, ylab = "Housing Age")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$Hage,  xlab = "Housing Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$Hage,  xlab = "Housing Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent silt
boxplot(sitr$Silt, ylab = "Silt")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$Silt,  xlab = "Silt",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$Silt,  xlab = "Silt",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent hardwood biomass
boxplot(sitr$ln.perH, ylab = "hardwood biomass")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$ln.perH,  xlab = "hardwood biomass",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$ln.perH,  xlab = "hardwood biomass",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#median income
boxplot(sitr$medINC, ylab = "Median Income")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$medINC,  xlab = "Median Income",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$medINC,  xlab = "Median Income",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#total assessed value
boxplot(sitr$TotalValua, ylab = "Assessed Value")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$TotalValua,  xlab = "Assessed Value",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$TotalValua,  xlab = "Assessed Value",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent vacant housing within census block
boxplot(sitr$perVAC, ylab = "Percent Vacant")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$perVAC,  xlab = "Percent Vacant",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$perVAC,  xlab = "Percent Vacant",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent married couples
boxplot(sitr$perMAR, ylab = "Perecent Married")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$perMAR,  xlab = "Percent Married",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$perMAR,  xlab = "Percent Married",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#residence duration
boxplot(sitr$dur, ylab = "Residence Duration")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$dur,  xlab = "Residence Duration",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$dur,  xlab = "Residence Duration",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#median resident age
boxplot(sitr$MEDIANAGE, ylab = "Median Age")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$MEDIANAGE,  xlab = "Median Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$MEDIANAGE,  xlab = "Median Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#yard size
boxplot(sitr$yd, ylab = "Yard Size")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$yd,  xlab = "Yard Size",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$tbio.allo.c), ]
dotchart(sitr$yd,  xlab = "Yard Size",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

######################
#reclassify variables#
######################

#make ID and Depth.Fraction in sites.scn categorical variables
sites_scn$ID = factor(sites_scn$ID)
sites_scn$Depth.Fraction = factor(sites_scn$Depth.Fraction)

#make ID and fiac in sitrcla categorical variables
sitrcla$ID = factor(sitrcla$ID)
sitrcla$fiac = factor(sitrcla$fiac)

#make new columns for aboveground biomass (tbio.allo.c), median income (medINC.1), and median age
#(medAGE.1) to remove outliers identified during preliminary data analysis
#outliers include aboveground biomass > 100,000 kg C / m2, median income < 40K > 100K, age > 60 

sitr$tbio.allo.c = ifelse(sitr$ID == 3 | sitr$ID == 35, NA, sitr$Cseqha)
sitr$medINC.1 = ifelse(sitr$medINC < 4e4 | sitr$medINC > 1e5, NA, sitr$medINC)
sitr$medAGE.1 = ifelse(sitr$MEDIANAGE > 60, NA, sitr$MEDIANAGE)

sitrcla$tbio.allo.c = ifelse(sitrcla$ID == 3 | sitrcla$ID == 35, NA, sitrcla$Cseqha)
sitrcla$medINC.1 = ifelse(sitrcla$medINC < 4e4 | sitrcla$medINC > 1e5, NA, sitrcla$medINC)
sitrcla$medAGE.1 = ifelse(sitrcla$MEDIANAGE > 60, NA, sitrcla$MEDIANAGE)

sitrloc$tbio.allo.c = ifelse(sitrloc$ID == 3 | sitrloc$ID == 35, NA, sitrloc$Cseqha)
sitrloc$medINC.1 = ifelse(sitrloc$medINC < 4e4 | sitrloc$medINC > 1e5, NA, sitrloc$medINC)
sitrloc$medAGE.1 = ifelse(sitrloc$MEDIANAGE > 60, NA, sitrloc$MEDIANAGE)

###############################################
#homogeneity of variance in response variables#
###############################################

#belowground soil carbon as a function of depth fraction
boxplot(stoC ~ Depth.Fraction, data = sites_scn)

#belowground soil carbon within depth fractions as a function of location in yard
boxplot(stoCloc_at10 ~ Loc, data = loc_sites)
boxplot(stoCloc_at20 ~ Loc, data = loc_sites)
boxplot(stoCloc_at30 ~ Loc, data = loc_sites)
boxplot(stoCloc_at40 ~ Loc, data = loc_sites)
boxplot(stoCloc_at50 ~ Loc, data = loc_sites)

#aboveground biomass carbon as a function of size class
boxplot(tbio.allo.c ~ fiac, data = sitrcla)

#aboveground biomass carbon as a function of location in yard
boxplot(tbio.allo.c ~ Loc, data = sitrloc)

####################################################################################
####################################################################################
#ANOVA-Type analyses of differences in belowground carbon stocks among depths
#and aboveground carbon stocks among size classes
####################################################################################
####################################################################################

###########################
#belowground carbon stocks#
###########################

#select random effects
gls.stoC = gls( stoC ~ Depth.Fraction, data = sites_scn, na.action = na.omit)
lme.stoC = lme(fixed = stoC ~ Depth.Fraction, random = ~1|ID, data = sites_scn, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.stoC, lme.stoC)
#model fit improved with ID as a random intercept

#select variance structures
var.stoC = update(lme.stoC, weights = varIdent(form = ~ 1 | Depth.Fraction))

#compare model fits
BIC(lme.stoC, var.stoC)
#no difference in model fit

#examine significance of fixed effects
anova(lme.stoC, type = "marginal")
#effect of depth fraction highly significant

#validate model by examining homogeneity and normality of residuals
plot(lme.stoC)
qqnorm(lme.stoC)

#obtain pairwise comparisons of differences between depth fractions
summary((glht(lme.stoC, linfct = mcp(Depth.Fraction = "Tukey"))))

###########################
#aboveground carbon stocks#
###########################

#select random effects
gls.treecla = gls(tbio.allo.c ~ fiac, data = sitrcla, na.action = na.omit)
lme.treecla = lme(fixed = tbio.allo.c ~ fiac, random = ~1|ID, data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.treecla, lme.treecla)
#model fit not improved with ID as a random intercept

#select variance structures
var.treecla = update(gls.treecla, weights = varIdent(form = ~ 1 | fiac))

#compare model fits
BIC(gls.treecla, var.treecla)
#model fit improved with fiac as a constant variance structure

#examine significance of fixed effects
anova(var.treecla, type = "marginal")
#effect of depth fraction highly significant

#validate model by examining homogeneity and normality of residuals
plot(var.treecla)
qqnorm(var.treecla)

#obtain pairwise comparisons of differences between depth fractions
summary((glht(var.treecla, linfct = mcp(fiac = "Tukey"))))

####################################################################################
####################################################################################
#ANOVA-Type analyses of differences in belowground and aboveground 
#carbon stocks between front and backyards
####################################################################################
####################################################################################

###########################
#belowground carbon stocks#
###########################

###########
#0 to 10 cm
###########

#select random effects
gls.stoC_loc_10 = gls(stoCloc_at10 ~ Loc, data = loc_sites, na.action = na.omit)
lme.stoC_loc_10 = lme(fixed = stoCloc_at10 ~ Loc, random = ~1|ID, data = loc_sites, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.stoC_loc_10, lme.stoC_loc_10)
#model fit improved with a random intercept

#select variance structures
var.stoC_loc_10 = update(lme.stoC_loc_10, weights = varIdent(form = ~ 1 | Loc))

#compare model fits
BIC(lme.stoC_loc_10, var.stoC_loc_10)
#no difference in model fit

#examine significance of fixed effects
anova(lme.stoC_loc_10, type = "marginal")
#effect of location not significant

#validate model by examining homogeneity and normality of residuals
plot(lme.stoC_loc_10)
qqnorm(lme.stoC_loc_10)

###########
#10 to 20 cm
###########

#select random effects
gls.stoC_loc_20 = gls(stoCloc_at20 ~ Loc, data = loc_sites, na.action = na.omit)
lme.stoC_loc_20 = lme(fixed = stoCloc_at20 ~ Loc, random = ~1|ID, data = loc_sites, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.stoC_loc_20, lme.stoC_loc_20)
#model fit improved with a random intercept

#select variance structures
var.stoC_loc_20 = update(lme.stoC_loc_20, weights = varIdent(form = ~ 1 | Loc))

#compare model fits
BIC(lme.stoC_loc_20, var.stoC_loc_20)
#no difference in model fit

#examine significance of fixed effects
anova(lme.stoC_loc_20, type = "marginal")
#effect of location not significant

#validate model by examining homogeneity and normality of residuals
plot(lme.stoC_loc_20)
qqnorm(lme.stoC_loc_20)

###########
#20 to 30 cm
###########

#select random effects
gls.stoC_loc_30 = gls(stoCloc_at30 ~ Loc, data = loc_sites, na.action = na.omit)
lme.stoC_loc_30 = lme(fixed = stoCloc_at30 ~ Loc, random = ~1|ID, data = loc_sites, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.stoC_loc_30, lme.stoC_loc_30)
#model fit improved with a random intercept

#select variance structures
var.stoC_loc_30 = update(lme.stoC_loc_30, weights = varIdent(form = ~ 1 | Loc))

#compare model fits
BIC(lme.stoC_loc_30, var.stoC_loc_30)
#no difference in model fit

#examine significance of fixed effects
anova(lme.stoC_loc_30, type = "marginal")
#effect of location not significant

#validate model by examining homogeneity and normality of residuals
plot(lme.stoC_loc_30)
qqnorm(lme.stoC_loc_30)

###########
#30 to 40 cm
###########

#select random effects
gls.stoC_loc_40 = gls(stoCloc_at40 ~ Loc, data = loc_sites, na.action = na.omit)
lme.stoC_loc_40 = lme(fixed = stoCloc_at40 ~ Loc, random = ~1|ID, data = loc_sites, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.stoC_loc_40, lme.stoC_loc_40)
#model fit improved with a random intercept

#select variance structures
var.stoC_loc_40 = update(lme.stoC_loc_40, weights = varIdent(form = ~ 1 | Loc))

#compare model fits
BIC(lme.stoC_loc_40, var.stoC_loc_40)
#no difference in model fit

#examine significance of fixed effects
anova(lme.stoC_loc_40, type = "marginal")
#effect of location not significant

#validate model by examining homogeneity and normality of residuals
plot(lme.stoC_loc_40)
qqnorm(lme.stoC_loc_40)

###########
#40 to 50 cm
###########

#select random effects
gls.stoC_loc_50 = gls(stoCloc_at50 ~ Loc, data = loc_sites, na.action = na.omit)
lme.stoC_loc_50 = lme(fixed = stoCloc_at50 ~ Loc, random = ~1|ID, data = loc_sites, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.stoC_loc_50, lme.stoC_loc_50)
#model fit not improved with a random intercept

#select variance structures
var.stoC_loc_50 = update(gls.stoC_loc_50, weights = varIdent(form = ~ 1 | Loc))

#compare model fits
BIC(gls.stoC_loc_50, var.stoC_loc_50)
#no difference in model fit

#examine significance of fixed effects
anova(gls.stoC_loc_50, type = "marginal")
#effect of location not significant

#validate model by examining homogeneity and normality of residuals
plot(gls.stoC_loc_50)
qqnorm(gls.stoC_loc_50)

#####################
#aboveground biomass
#####################

#select random effects
gls.Cseqha = gls(tbio.allo.c ~ Loc, data = sitrloc, na.action = na.omit)
lme.Cseqha = lme(fixed = tbio.allo.c ~ Loc, random = ~1|ID, data = sitrloc, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.Cseqha, lme.Cseqha)
#model fit not improved with a random intercept

#select variance structures
var.Cseqha = update(gls.Cseqha, weights = varIdent(form = ~ 1 | Loc))

#compare model fits
BIC(gls.Cseqha, var.Cseqha)
#no difference in model fit

#examine significance of fixed effects
anova(gls.Cseqha, type = "marginal")
#effect of depth fraction highly significant

#validate model by examining homogeneity and normality of residuals
plot(gls.Cseqha)
qqnorm(gls.Cseqha)

################################################################################
################################################################################
#Regression-type models for belowground stocks as a function of biogeochemical 
#and socioeconomic variables across depths
################################################################################
################################################################################

############
############
#0 to 10 cm#
############
############

#############
#housing age#
#############

#select random effects
gls.HageC.10 = gls(stoC_at10 ~ Hage, data = sitr, na.action = na.omit)
lme.HageC.10 = lme(fixed = stoC_at10 ~ Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.10, lme.HageC.10)
#no difference in model fit

#use linear model
lm.HageC.10 = lm(stoC_at10 ~ Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.10)

################
#housing age ^2#
################

#select random effects
gls.HageC.102 = gls(stoC_at10 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.HageC.102 = lme(fixed = stoC_at10 ~ Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.102, lme.HageC.102)
#no difference in model fit

#use linear model
lm.HageC.102 = lm(stoC_at10 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.102)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.10 = gls(stoC_at10 ~ POP_Densit, data = sitr, na.action = na.omit)
lme.POP_DensitC.10 = lme(fixed = stoC_at10 ~ POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.10, lme.POP_DensitC.10)
#no difference in model fit

#use linear model
lm.POP_DensitC.10 = lm(stoC_at10 ~ POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.10)

##############
#percent silt#
##############

#select random effects
gls.SiltC.10 = gls(stoC_at10 ~ Silt, data = sitr, na.action = na.omit)
lme.SiltC.10 = lme(fixed = stoC_at10 ~ Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.10, lme.SiltC.10)
#no difference in model fit

#use linear model
lm.SiltC.10 = lm(stoC_at10 ~ Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.10)

##################
#hardwood biomass#
##################

#select random effects
gls.ln.perHC.10 = gls(stoC_at10 ~ ln.perH, data = sitr, na.action = na.omit)
lme.ln.perHC.10 = lme(fixed = stoC_at10 ~ ln.perH, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ln.perHC.10, lme.ln.perHC.10)
#no difference in model fit

#use linear model
lm.ln.perHC.10 = lm(stoC_at10 ~ ln.perH, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ln.perHC.10)

###############
#median income#
###############

#select random effects
gls.medINC.1C.10 = gls(stoC_at10 ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C.10 = lme(fixed = stoC_at10 ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.10, lme.medINC.1C.10)
#no difference in model fit

#use linear model
lm.medINC.1C.10 = lm(stoC_at10 ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.10)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.10 = gls(stoC_at10 ~ TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuaC.10 = lme(fixed = stoC_at10 ~ TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.10, lme.TotalValuaC.10)
#no difference in model fit

#use linear model
lm.TotalValuaC.10 = lm(stoC_at10 ~ TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.10)

################
#percent vacant#
################

#select random effects
gls.perVACC.10 = gls(stoC_at10 ~ perVAC, data = sitr, na.action = na.omit)
lme.perVACC.10 = lme(fixed = stoC_at10 ~ perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.10, lme.perVACC.10)
#no difference in model fit

#use linear model
lm.perVACC.10 = lm(stoC_at10 ~ perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.10)

##################
#percent minority#
##################

#select random effects
gls.perMINC.10 = gls(stoC_at10 ~ perMIN, data = sitr, na.action = na.omit)
lme.perMINC.10 = lme(fixed = stoC_at10 ~ perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.10, lme.perMINC.10)
#no difference in model fit

#use linear model
lm.perMINC.10 = lm(stoC_at10 ~ perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.10)

#################
#percent married#
#################

#select random effects
gls.perMARC.10 = gls(stoC_at10 ~ perMAR, data = sitr, na.action = na.omit)
lme.perMARC.10 = lme(fixed = stoC_at10 ~ perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.10, lme.perMARC.10)
#no difference in model fit

#use linear model
lm.perMARC.10 = lm(stoC_at10 ~ perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.10)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.10 = gls(stoC_at10 ~ medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1C.10 = lme(fixed = stoC_at10 ~ medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.10, lme.medAGE.1C.10)
#no difference in model fit

#use linear model
lm.medAGE.1C.10 = lm(stoC_at10 ~ medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.10)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.10 = gls(stoC_at10 ~ dur, data = sitr, na.action = na.omit)
lme.durC.10 = lme(fixed = stoC_at10 ~ dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.10, lme.durC.10)
#no difference in model fit

#use linear model
lm.durC.10 = lm(stoC_at10 ~ dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.10)

###########
#yard size#
###########

#select random effects
gls.ydC.10 = gls(stoC_at10 ~ yd, data = sitr, na.action = na.omit)
lme.ydC.10 = lme(fixed = stoC_at10 ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.10, lme.ydC.10)
#no difference in model fit

#use linear model
lm.ydC.10 = lm(stoC_at10 ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.10)

#############
#############
#10 to 20 cm#
#############
#############

#############
#housing age#
#############

#select random effects
gls.HageC.20 = gls(stoC_at20 ~ Hage, data = sitr, na.action = na.omit)
lme.HageC.20 = lme(fixed = stoC_at20 ~ Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.20, lme.HageC.20)
#no difference in model fit

#use linear model
lm.HageC.20 = lm(stoC_at20 ~ Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.20)

################
#housing age ^2#
################

#select random effects
gls.HageC.202 = gls(stoC_at20 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.HageC.202 = lme(fixed = stoC_at20 ~ Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.202, lme.HageC.202)
#no difference in model fit

#use linear model
lm.HageC.202 = lm(stoC_at20 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.202)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.20 = gls(stoC_at20 ~ POP_Densit, data = sitr, na.action = na.omit)
lme.POP_DensitC.20 = lme(fixed = stoC_at20 ~ POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.20, lme.POP_DensitC.20)
#no difference in model fit

#use linear model
lm.POP_DensitC.20 = lm(stoC_at20 ~ POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.20)

##############
#percent silt#
##############

#select random effects
gls.SiltC.20 = gls(stoC_at20 ~ Silt, data = sitr, na.action = na.omit)
lme.SiltC.20 = lme(fixed = stoC_at20 ~ Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.20, lme.SiltC.20)
#no difference in model fit

#use linear model
lm.SiltC.20 = lm(stoC_at20 ~ Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.20)

##################
#hardwood biomass#
##################

#select random effects
gls.ln.perHC.20 = gls(stoC_at20 ~ ln.perH, data = sitr, na.action = na.omit)
lme.ln.perHC.20 = lme(fixed = stoC_at20 ~ ln.perH, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ln.perHC.20, lme.ln.perHC.20)
#no difference in model fit

#use linear model
lm.ln.perHC.20 = lm(stoC_at20 ~ ln.perH, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ln.perHC.20)

###############
#median income#
###############

#select random effects
gls.medINC.1C.20 = gls(stoC_at20 ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C.20 = lme(fixed = stoC_at20 ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.20, lme.medINC.1C.20)
#no difference in model fit

#use linear model
lm.medINC.1C.20 = lm(stoC_at20 ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.20)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.20 = gls(stoC_at20 ~ TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuaC.20 = lme(fixed = stoC_at20 ~ TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.20, lme.TotalValuaC.20)
#no difference in model fit

#use linear model
lm.TotalValuaC.20 = lm(stoC_at20 ~ TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.20)

################
#percent vacant#
################

#select random effects
gls.perVACC.20 = gls(stoC_at20 ~ perVAC, data = sitr, na.action = na.omit)
lme.perVACC.20 = lme(fixed = stoC_at20 ~ perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.20, lme.perVACC.20)
#no difference in model fit

#use linear model
lm.perVACC.20 = lm(stoC_at20 ~ perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.20)

##################
#percent minority#
##################

#select random effects
gls.perMINC.20 = gls(stoC_at20 ~ perMIN, data = sitr, na.action = na.omit)
lme.perMINC.20 = lme(fixed = stoC_at20 ~ perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.20, lme.perMINC.20)
#no difference in model fit

#use linear model
lm.perMINC.20 = lm(stoC_at20 ~ perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.20)

#################
#percent married#
#################

#select random effects
gls.perMARC.20 = gls(stoC_at20 ~ perMAR, data = sitr, na.action = na.omit)
lme.perMARC.20 = lme(fixed = stoC_at20 ~ perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.20, lme.perMARC.20)
#no difference in model fit

#use linear model
lm.perMARC.20 = lm(stoC_at20 ~ perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.20)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.20 = gls(stoC_at20 ~ medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1C.20 = lme(fixed = stoC_at20 ~ medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.20, lme.medAGE.1C.20)
#no difference in model fit

#use linear model
lm.medAGE.1C.20 = lm(stoC_at20 ~ medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.20)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.20 = gls(stoC_at20 ~ dur, data = sitr, na.action = na.omit)
lme.durC.20 = lme(fixed = stoC_at20 ~ dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.20, lme.durC.20)
#no difference in model fit

#use linear model
lm.durC.20 = lm(stoC_at20 ~ dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.20)

###########
#yard size#
###########

#select random effects
gls.ydC.20 = gls(stoC_at20 ~ yd, data = sitr, na.action = na.omit)
lme.ydC.20 = lme(fixed = stoC_at20 ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.20, lme.ydC.20)
#no difference in model fit

#use linear model
lm.ydC.20 = lm(stoC_at20 ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.20)

#############
#############
#20 to 30 cm#
#############
#############

#############
#housing age#
#############

#select random effects
gls.HageC.30 = gls(stoC_at30 ~ Hage, data = sitr, na.action = na.omit)
lme.HageC.30 = lme(fixed = stoC_at30 ~ Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.30, lme.HageC.30)
#no difference in model fit

#use linear model
lm.HageC.30 = lm(stoC_at30 ~ Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.30)

################
#housing age ^2#
################

#select random effects
gls.HageC.302 = gls(stoC_at30 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.HageC.302 = lme(fixed = stoC_at30 ~ Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.302, lme.HageC.302)
#no difference in model fit

#use linear model
lm.HageC.302 = lm(stoC_at30 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.302)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.30 = gls(stoC_at30 ~ POP_Densit, data = sitr, na.action = na.omit)
lme.POP_DensitC.30 = lme(fixed = stoC_at30 ~ POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.30, lme.POP_DensitC.30)
#no difference in model fit

#use linear model
lm.POP_DensitC.30 = lm(stoC_at30 ~ POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.30)

##############
#percent silt#
##############

#select random effects
gls.SiltC.30 = gls(stoC_at30 ~ Silt, data = sitr, na.action = na.omit)
lme.SiltC.30 = lme(fixed = stoC_at30 ~ Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.30, lme.SiltC.30)
#no difference in model fit

#use linear model
lm.SiltC.30 = lm(stoC_at30 ~ Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.30)

##################
#hardwood biomass#
##################

#select random effects
gls.ln.perHC.30 = gls(stoC_at30 ~ ln.perH, data = sitr, na.action = na.omit)
lme.ln.perHC.30 = lme(fixed = stoC_at30 ~ ln.perH, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ln.perHC.30, lme.ln.perHC.30)
#no difference in model fit

#use linear model
lm.ln.perHC.30 = lm(stoC_at30 ~ ln.perH, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ln.perHC.30)

###############
#median income#
###############

#select random effects
gls.medINC.1C.30 = gls(stoC_at30 ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C.30 = lme(fixed = stoC_at30 ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.30, lme.medINC.1C.30)
#no difference in model fit

#use linear model
lm.medINC.1C.30 = lm(stoC_at30 ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.30)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.30 = gls(stoC_at30 ~ TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuaC.30 = lme(fixed = stoC_at30 ~ TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.30, lme.TotalValuaC.30)
#no difference in model fit

#use linear model
lm.TotalValuaC.30 = lm(stoC_at30 ~ TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.30)

################
#percent vacant#
################

#select random effects
gls.perVACC.30 = gls(stoC_at30 ~ perVAC, data = sitr, na.action = na.omit)
lme.perVACC.30 = lme(fixed = stoC_at30 ~ perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.30, lme.perVACC.30)
#no difference in model fit

#use linear model
lm.perVACC.30 = lm(stoC_at30 ~ perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.30)

##################
#percent minority#
##################

#select random effects
gls.perMINC.30 = gls(stoC_at30 ~ perMIN, data = sitr, na.action = na.omit)
lme.perMINC.30 = lme(fixed = stoC_at30 ~ perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.30, lme.perMINC.30)
#no difference in model fit

#use linear model
lm.perMINC.30 = lm(stoC_at30 ~ perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.30)

#################
#percent married#
#################

#select random effects
gls.perMARC.30 = gls(stoC_at30 ~ perMAR, data = sitr, na.action = na.omit)
lme.perMARC.30 = lme(fixed = stoC_at30 ~ perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.30, lme.perMARC.30)
#no difference in model fit

#use linear model
lm.perMARC.30 = lm(stoC_at30 ~ perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.30)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.30 = gls(stoC_at30 ~ medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1C.30 = lme(fixed = stoC_at30 ~ medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.30, lme.medAGE.1C.30)
#no difference in model fit

#use linear model
lm.medAGE.1C.30 = lm(stoC_at30 ~ medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.30)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.30 = gls(stoC_at30 ~ dur, data = sitr, na.action = na.omit)
lme.durC.30 = lme(fixed = stoC_at30 ~ dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.30, lme.durC.30)
#no difference in model fit

#use linear model
lm.durC.30 = lm(stoC_at30 ~ dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.30)

###########
#yard size#
###########

#select random effects
gls.ydC.30 = gls(stoC_at30 ~ yd, data = sitr, na.action = na.omit)
lme.ydC.30 = lme(fixed = stoC_at30 ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.30, lme.ydC.30)
#no difference in model fit

#use linear model
lm.ydC.30 = lm(stoC_at30 ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.30)

#############
#############
#30 to 40 cm#
#############
#############

#############
#housing age#
#############

#select random effects
gls.HageC.40 = gls(stoC_at40 ~ Hage, data = sitr, na.action = na.omit)
lme.HageC.40 = lme(fixed = stoC_at40 ~ Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.40, lme.HageC.40)
#no difference in model fit

#use linear model
lm.HageC.40 = lm(stoC_at40 ~ Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.40)

################
#housing age ^2#
################

#select random effects
gls.HageC.402 = gls(stoC_at40 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.HageC.402 = lme(fixed = stoC_at40 ~ Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.402, lme.HageC.402)
#no difference in model fit

#use linear model
lm.HageC.402 = lm(stoC_at40 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.402)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.40 = gls(stoC_at40 ~ POP_Densit, data = sitr, na.action = na.omit)
lme.POP_DensitC.40 = lme(fixed = stoC_at40 ~ POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.40, lme.POP_DensitC.40)
#no difference in model fit

#use linear model
lm.POP_DensitC.40 = lm(stoC_at40 ~ POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.40)

##############
#percent silt#
##############

#select random effects
gls.SiltC.40 = gls(stoC_at40 ~ Silt, data = sitr, na.action = na.omit)
lme.SiltC.40 = lme(fixed = stoC_at40 ~ Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.40, lme.SiltC.40)
#no difference in model fit

#use linear model
lm.SiltC.40 = lm(stoC_at40 ~ Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.40)

##################
#hardwood biomass#
##################

#select random effects
gls.ln.perHC.40 = gls(stoC_at40 ~ ln.perH, data = sitr, na.action = na.omit)
lme.ln.perHC.40 = lme(fixed = stoC_at40 ~ ln.perH, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ln.perHC.40, lme.ln.perHC.40)
#no difference in model fit

#use linear model
lm.ln.perHC.40 = lm(stoC_at40 ~ ln.perH, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ln.perHC.40)

###############
#median income#
###############

#select random effects
gls.medINC.1C.40 = gls(stoC_at40 ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C.40 = lme(fixed = stoC_at40 ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.40, lme.medINC.1C.40)
#no difference in model fit

#use linear model
lm.medINC.1C.40 = lm(stoC_at40 ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.40)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.40 = gls(stoC_at40 ~ TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuaC.40 = lme(fixed = stoC_at40 ~ TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.40, lme.TotalValuaC.40)
#no difference in model fit

#use linear model
lm.TotalValuaC.40 = lm(stoC_at40 ~ TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.40)

################
#percent vacant#
################

#select random effects
gls.perVACC.40 = gls(stoC_at40 ~ perVAC, data = sitr, na.action = na.omit)
lme.perVACC.40 = lme(fixed = stoC_at40 ~ perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.40, lme.perVACC.40)
#no difference in model fit

#use linear model
lm.perVACC.40 = lm(stoC_at40 ~ perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.40)

##################
#percent minority#
##################

#select random effects
gls.perMINC.40 = gls(stoC_at40 ~ perMIN, data = sitr, na.action = na.omit)
lme.perMINC.40 = lme(fixed = stoC_at40 ~ perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.40, lme.perMINC.40)
#no difference in model fit

#use linear model
lm.perMINC.40 = lm(stoC_at40 ~ perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.40)

#################
#percent married#
#################

#select random effects
gls.perMARC.40 = gls(stoC_at40 ~ perMAR, data = sitr, na.action = na.omit)
lme.perMARC.40 = lme(fixed = stoC_at40 ~ perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.40, lme.perMARC.40)
#no difference in model fit

#use linear model
lm.perMARC.40 = lm(stoC_at40 ~ perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.40)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.40 = gls(stoC_at40 ~ medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1C.40 = lme(fixed = stoC_at40 ~ medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.40, lme.medAGE.1C.40)
#no difference in model fit

#use linear model
lm.medAGE.1C.40 = lm(stoC_at40 ~ medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.40)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.40 = gls(stoC_at40 ~ dur, data = sitr, na.action = na.omit)
lme.durC.40 = lme(fixed = stoC_at40 ~ dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.40, lme.durC.40)
#no difference in model fit

#use linear model
lm.durC.40 = lm(stoC_at40 ~ dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.40)

###########
#yard size#
###########

#select random effects
gls.ydC.40 = gls(stoC_at40 ~ yd, data = sitr, na.action = na.omit)
lme.ydC.40 = lme(fixed = stoC_at40 ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.40, lme.ydC.40)
#no difference in model fit

#use linear model
lm.ydC.40 = lm(stoC_at40 ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.40)

#############
#############
#40 to 50 cm#
#############
#############

#############
#housing age#
#############

#select random effects
gls.HageC.50 = gls(stoC_at50 ~ Hage, data = sitr, na.action = na.omit)
lme.HageC.50 = lme(fixed = stoC_at50 ~ Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.50, lme.HageC.50)
#no difference in model fit

#use linear model
lm.HageC.50 = lm(stoC_at50 ~ Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.50)

################
#housing age ^2#
################

#select random effects
gls.HageC.502 = gls(stoC_at50 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.HageC.502 = lme(fixed = stoC_at50 ~ Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.502, lme.HageC.502)
#no difference in model fit

#use linear model
lm.HageC.502 = lm(stoC_at50 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.502)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.50 = gls(stoC_at50 ~ POP_Densit, data = sitr, na.action = na.omit)
lme.POP_DensitC.50 = lme(fixed = stoC_at50 ~ POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.50, lme.POP_DensitC.50)
#no difference in model fit

#use linear model
lm.POP_DensitC.50 = lm(stoC_at50 ~ POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.50)

##############
#percent silt#
##############

#select random effects
gls.SiltC.50 = gls(stoC_at50 ~ Silt, data = sitr, na.action = na.omit)
lme.SiltC.50 = lme(fixed = stoC_at50 ~ Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.50, lme.SiltC.50)
#no difference in model fit

#use linear model
lm.SiltC.50 = lm(stoC_at50 ~ Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.50)

##################
#hardwood biomass#
##################

#select random effects
gls.ln.perHC.50 = gls(stoC_at50 ~ ln.perH, data = sitr, na.action = na.omit)
lme.ln.perHC.50 = lme(fixed = stoC_at50 ~ ln.perH, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ln.perHC.50, lme.ln.perHC.50)
#no difference in model fit

#use linear model
lm.ln.perHC.50 = lm(stoC_at50 ~ ln.perH, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ln.perHC.50)

###############
#median income#
###############

#select random effects
gls.medINC.1C.50 = gls(stoC_at50 ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C.50 = lme(fixed = stoC_at50 ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.50, lme.medINC.1C.50)
#no difference in model fit

#use linear model
lm.medINC.1C.50 = lm(stoC_at50 ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.50)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.50 = gls(stoC_at50 ~ TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuaC.50 = lme(fixed = stoC_at50 ~ TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.50, lme.TotalValuaC.50)
#no difference in model fit

#use linear model
lm.TotalValuaC.50 = lm(stoC_at50 ~ TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.50)

################
#percent vacant#
################

#select random effects
gls.perVACC.50 = gls(stoC_at50 ~ perVAC, data = sitr, na.action = na.omit)
lme.perVACC.50 = lme(fixed = stoC_at50 ~ perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.50, lme.perVACC.50)
#no difference in model fit

#use linear model
lm.perVACC.50 = lm(stoC_at50 ~ perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.50)

##################
#percent minority#
##################

#select random effects
gls.perMINC.50 = gls(stoC_at50 ~ perMIN, data = sitr, na.action = na.omit)
lme.perMINC.50 = lme(fixed = stoC_at50 ~ perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.50, lme.perMINC.50)
#no difference in model fit

#use linear model
lm.perMINC.50 = lm(stoC_at50 ~ perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.50)

#################
#percent married#
#################

#select random effects
gls.perMARC.50 = gls(stoC_at50 ~ perMAR, data = sitr, na.action = na.omit)
lme.perMARC.50 = lme(fixed = stoC_at50 ~ perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.50, lme.perMARC.50)
#no difference in model fit

#use linear model
lm.perMARC.50 = lm(stoC_at50 ~ perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.50)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.50 = gls(stoC_at50 ~ medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1C.50 = lme(fixed = stoC_at50 ~ medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.50, lme.medAGE.1C.50)
#no difference in model fit

#use linear model
lm.medAGE.1C.50 = lm(stoC_at50 ~ medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.50)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.50 = gls(stoC_at50 ~ dur, data = sitr, na.action = na.omit)
lme.durC.50 = lme(fixed = stoC_at50 ~ dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.50, lme.durC.50)
#no difference in model fit

#use linear model
lm.durC.50 = lm(stoC_at50 ~ dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.50)

###########
#yard size#
###########

#select random effects
gls.ydC.50 = gls(stoC_at50 ~ yd, data = sitr, na.action = na.omit)
lme.ydC.50 = lme(fixed = stoC_at50 ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.50, lme.ydC.50)
#no difference in model fit

#use linear model
lm.ydC.50 = lm(stoC_at50 ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.50)

############
############
#0 to 50 cm#
############
############

#############
#housing age#
#############

#select random effects
gls.HageC.50 = gls(stoC_to50 ~ Hage, data = sitr, na.action = na.omit)
lme.HageC.50 = lme(fixed = stoC_to50 ~ Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.50, lme.HageC.50)
#no difference in model fit

#use linear model
lm.HageC.50 = lm(stoC_to50 ~ Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.50)

################
#housing age ^2#
################

#select random effects
gls.HageC.502 = gls(stoC_to50 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.HageC.502 = lme(fixed = stoC_to50 ~ Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.502, lme.HageC.502)
#no difference in model fit

#use linear model
lm.HageC.502 = lm(stoC_to50 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.502)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.50 = gls(stoC_to50 ~ POP_Densit, data = sitr, na.action = na.omit)
lme.POP_DensitC.50 = lme(fixed = stoC_to50 ~ POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.50, lme.POP_DensitC.50)
#no difference in model fit

#use linear model
lm.POP_DensitC.50 = lm(stoC_to50 ~ POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.50)

##############
#percent silt#
##############

#select random effects
gls.SiltC.50 = gls(stoC_to50 ~ Silt, data = sitr, na.action = na.omit)
lme.SiltC.50 = lme(fixed = stoC_to50 ~ Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.50, lme.SiltC.50)
#no difference in model fit

#use linear model
lm.SiltC.50 = lm(stoC_to50 ~ Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.50)

##################
#hardwood biomass#
##################

#select random effects
gls.ln.perHC.50 = gls(stoC_to50 ~ ln.perH, data = sitr, na.action = na.omit)
lme.ln.perHC.50 = lme(fixed = stoC_to50 ~ ln.perH, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ln.perHC.50, lme.ln.perHC.50)
#no difference in model fit

#use linear model
lm.ln.perHC.50 = lm(stoC_to50 ~ ln.perH, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ln.perHC.50)

###############
#median income#
###############

#select random effects
gls.medINC.1C.50 = gls(stoC_to50 ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C.50 = lme(fixed = stoC_to50 ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.50, lme.medINC.1C.50)
#no difference in model fit

#use linear model
lm.medINC.1C.50 = lm(stoC_to50 ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.50)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.50 = gls(stoC_to50 ~ TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuaC.50 = lme(fixed = stoC_to50 ~ TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.50, lme.TotalValuaC.50)
#no difference in model fit

#use linear model
lm.TotalValuaC.50 = lm(stoC_to50 ~ TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.50)

################
#percent vacant#
################

#select random effects
gls.perVACC.50 = gls(stoC_to50 ~ perVAC, data = sitr, na.action = na.omit)
lme.perVACC.50 = lme(fixed = stoC_to50 ~ perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.50, lme.perVACC.50)
#no difference in model fit

#use linear model
lm.perVACC.50 = lm(stoC_to50 ~ perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.50)

##################
#percent minority#
##################

#select random effects
gls.perMINC.50 = gls(stoC_to50 ~ perMIN, data = sitr, na.action = na.omit)
lme.perMINC.50 = lme(fixed = stoC_to50 ~ perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.50, lme.perMINC.50)
#no difference in model fit

#use linear model
lm.perMINC.50 = lm(stoC_to50 ~ perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.50)

#################
#percent married#
#################

#select random effects
gls.perMARC.50 = gls(stoC_to50 ~ perMAR, data = sitr, na.action = na.omit)
lme.perMARC.50 = lme(fixed = stoC_to50 ~ perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.50, lme.perMARC.50)
#no difference in model fit

#use linear model
lm.perMARC.50 = lm(stoC_to50 ~ perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.50)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.50 = gls(stoC_to50 ~ medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1C.50 = lme(fixed = stoC_to50 ~ medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.50, lme.medAGE.1C.50)
#no difference in model fit

#use linear model
lm.medAGE.1C.50 = lm(stoC_to50 ~ medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.50)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.50 = gls(stoC_to50 ~ dur, data = sitr, na.action = na.omit)
lme.durC.50 = lme(fixed = stoC_to50 ~ dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.50, lme.durC.50)
#no difference in model fit

#use linear model
lm.durC.50 = lm(stoC_to50 ~ dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.50)

###########
#yard size#
###########

#select random effects
gls.ydC.50 = gls(stoC_to50 ~ yd, data = sitr, na.action = na.omit)
lme.ydC.50 = lme(fixed = stoC_to50 ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.50, lme.ydC.50)
#no difference in model fit

#use linear model
lm.ydC.50 = lm(stoC_to50 ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.50)

################################################################################
################################################################################
#Regression-type models for aboveground stocks as a function of biogeochemical 
#and socioeconomic variables across size classes
################################################################################
################################################################################

#########
#########
#Sapling#
#########
#########

#############
#housing age#
#############

#select random effects
gls.HageC.SAP = gls(tbio.allo.c ~ Hage, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.HageC.SAP = lme(fixed = tbio.allo.c ~ Hage, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.SAP, lme.HageC.SAP)
#no difference in model fit

#use linear model
lm.HageC.SAP = lm(tbio.allo.c ~ Hage, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.SAP)

################
#housing age ^2#
################

#select random effects
gls.HageC.SAP2 = gls(tbio.allo.c ~ Hage + I(Hage^2), subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.HageC.SAP2 = lme(fixed = tbio.allo.c ~ Hage + I(Hage^2), random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.SAP2, lme.HageC.SAP2)
#no difference in model fit

#use linear model
lm.HageC.SAP2 = lm(tbio.allo.c ~ Hage + I(Hage^2), subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.SAP2)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.SAP = gls(tbio.allo.c ~ POP_Densit, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.POP_DensitC.SAP = lme(fixed = tbio.allo.c ~ POP_Densit, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.SAP, lme.POP_DensitC.SAP)
#no difference in model fit

#use linear model
lm.POP_DensitC.SAP = lm(tbio.allo.c ~ POP_Densit, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.SAP)

##############
#percent silt#
##############

#select random effects
gls.SiltC.SAP = gls(tbio.allo.c ~ Silt, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.SiltC.SAP = lme(fixed = tbio.allo.c ~ Silt, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.SAP, lme.SiltC.SAP)
#no difference in model fit

#use linear model
lm.SiltC.SAP = lm(tbio.allo.c ~ Silt, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.SAP)

###############
#median income#
###############

#select random effects
gls.medINC.1C.SAP = gls(tbio.allo.c ~ medINC.1, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.medINC.1C.SAP = lme(fixed = tbio.allo.c ~ medINC.1, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.SAP, lme.medINC.1C.SAP)
#no difference in model fit

#use linear model
lm.medINC.1C.SAP = lm(tbio.allo.c ~ medINC.1, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.SAP)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.SAP = gls(tbio.allo.c ~ TotalValua, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.TotalValuaC.SAP = lme(fixed = tbio.allo.c ~ TotalValua, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.SAP, lme.TotalValuaC.SAP)
#no difference in model fit

#use linear model
lm.TotalValuaC.SAP = lm(tbio.allo.c ~ TotalValua, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.SAP)

################
#percent vacant#
################

#select random effects
gls.perVACC.SAP = gls(tbio.allo.c ~ perVAC, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.perVACC.SAP = lme(fixed = tbio.allo.c ~ perVAC, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.SAP, lme.perVACC.SAP)
#no difference in model fit

#use linear model
lm.perVACC.SAP = lm(tbio.allo.c ~ perVAC, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.SAP)

##################
#percent minority#
##################

#select random effects
gls.perMINC.SAP = gls(tbio.allo.c ~ perMIN, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.perMINC.SAP = lme(fixed = tbio.allo.c ~ perMIN, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.SAP, lme.perMINC.SAP)
#no difference in model fit

#use linear model
lm.perMINC.SAP = lm(tbio.allo.c ~ perMIN, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.SAP)

#################
#percent married#
#################

#select random effects
gls.perMARC.SAP = gls(tbio.allo.c ~ perMAR, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.perMARC.SAP = lme(fixed = tbio.allo.c ~ perMAR, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.SAP, lme.perMARC.SAP)
#no difference in model fit

#use linear model
lm.perMARC.SAP = lm(tbio.allo.c ~ perMAR, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.SAP)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.SAP = gls(tbio.allo.c ~ medAGE.1, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.medAGE.1C.SAP = lme(fixed = tbio.allo.c ~ medAGE.1, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.SAP, lme.medAGE.1C.SAP)
#no difference in model fit

#use linear model
lm.medAGE.1C.SAP = lm(tbio.allo.c ~ medAGE.1, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.SAP)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.SAP = gls(tbio.allo.c ~ dur, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.durC.SAP = lme(fixed = tbio.allo.c ~ dur, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.SAP, lme.durC.SAP)
#no difference in model fit

#use linear model
lm.durC.SAP = lm(tbio.allo.c ~ dur, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.SAP)

###########
#yard size#
###########

#select random effects
gls.ydC.SAP = gls(tbio.allo.c ~ yd, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.ydC.SAP = lme(fixed = tbio.allo.c ~ yd, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.SAP, lme.ydC.SAP)
#no difference in model fit

#use linear model
lm.ydC.SAP = lm(tbio.allo.c ~ yd, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.SAP)

#############
#############
#Pole Timber#
#############
#############

#############
#housing age#
#############

#select random effects
gls.HageC.POL = gls(tbio.allo.c ~ Hage, subset = sitrcla$fiac == "POL", data = sitrcla, na.action = na.omit)
lme.HageC.POL = lme(fixed = tbio.allo.c ~ Hage, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.POL, lme.HageC.POL)
#no difference in model fit

#use linear model
lm.HageC.POL = lm(tbio.allo.c ~ Hage, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.POL)

################
#housing age ^2#
################

#select random effects
gls.HageC.POL2 = gls(tbio.allo.c ~ Hage + I(Hage^2), subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.HageC.POL2 = lme(fixed = tbio.allo.c ~ Hage + I(Hage^2), random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.POL2, lme.HageC.POL2)
#no difference in model fit

#use linear model
lm.HageC.POL2 = lm(tbio.allo.c ~ Hage + I(Hage^2), subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.POL2)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.POL = gls(tbio.allo.c ~ POP_Densit, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.POP_DensitC.POL = lme(fixed = tbio.allo.c ~ POP_Densit, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.POL, lme.POP_DensitC.POL)
#no difference in model fit

#use linear model
lm.POP_DensitC.POL = lm(tbio.allo.c ~ POP_Densit, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.POL)

##############
#percent silt#
##############

#select random effects
gls.SiltC.POL = gls(tbio.allo.c ~ Silt, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.SiltC.POL = lme(fixed = tbio.allo.c ~ Silt, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.POL, lme.SiltC.POL)
#no difference in model fit

#use linear model
lm.SiltC.POL = lm(tbio.allo.c ~ Silt, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.POL)

###############
#median income#
###############

#select random effects
gls.medINC.1C.POL = gls(tbio.allo.c ~ medINC.1, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.medINC.1C.POL = lme(fixed = tbio.allo.c ~ medINC.1, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.POL, lme.medINC.1C.POL)
#no difference in model fit

#use linear model
lm.medINC.1C.POL = lm(tbio.allo.c ~ medINC.1, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.POL)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.POL = gls(tbio.allo.c ~ TotalValua, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.TotalValuaC.POL = lme(fixed = tbio.allo.c ~ TotalValua, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.POL, lme.TotalValuaC.POL)
#no difference in model fit

#use linear model
lm.TotalValuaC.POL = lm(tbio.allo.c ~ TotalValua, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.POL)

################
#percent vacant#
################

#select random effects
gls.perVACC.POL = gls(tbio.allo.c ~ perVAC, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.perVACC.POL = lme(fixed = tbio.allo.c ~ perVAC, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.POL, lme.perVACC.POL)
#no difference in model fit

#use linear model
lm.perVACC.POL = lm(tbio.allo.c ~ perVAC, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.POL)

##################
#percent minority#
##################

#select random effects
gls.perMINC.POL = gls(tbio.allo.c ~ perMIN, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.perMINC.POL = lme(fixed = tbio.allo.c ~ perMIN, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.POL, lme.perMINC.POL)
#no difference in model fit

#use linear model
lm.perMINC.POL = lm(tbio.allo.c ~ perMIN, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.POL)

#################
#percent married#
#################

#select random effects
gls.perMARC.POL = gls(tbio.allo.c ~ perMAR, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.perMARC.POL = lme(fixed = tbio.allo.c ~ perMAR, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.POL, lme.perMARC.POL)
#no difference in model fit

#use linear model
lm.perMARC.POL = lm(tbio.allo.c ~ perMAR, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.POL)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.POL = gls(tbio.allo.c ~ medAGE.1, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.medAGE.1C.POL = lme(fixed = tbio.allo.c ~ medAGE.1, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.POL, lme.medAGE.1C.POL)
#no difference in model fit

#use linear model
lm.medAGE.1C.POL = lm(tbio.allo.c ~ medAGE.1, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.POL)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.POL = gls(tbio.allo.c ~ dur, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.durC.POL = lme(fixed = tbio.allo.c ~ dur, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.POL, lme.durC.POL)
#no difference in model fit

#use linear model
lm.durC.POL = lm(tbio.allo.c ~ dur, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.POL)

###########
#yard size#
###########

#select random effects
gls.ydC.POL = gls(tbio.allo.c ~ yd, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.ydC.POL = lme(fixed = tbio.allo.c ~ yd, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.POL, lme.ydC.POL)
#no difference in model fit

#use linear model
lm.ydC.POL = lm(tbio.allo.c ~ yd, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.POL)

#############
############
#Saw Timber#
############
############

#############
#housing age#
#############

#select random effects
gls.HageC.SAW = gls(tbio.allo.c ~ Hage, subset = sitrcla$fiac == "SAW", data = sitrcla, na.action = na.omit)
lme.HageC.SAW = lme(fixed = tbio.allo.c ~ Hage, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.SAW, lme.HageC.SAW)
#no difference in model fit

#use linear model
lm.HageC.SAW = lm(tbio.allo.c ~ Hage, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.SAW)

################
#housing age ^2#
################

#select random effects
gls.HageC.SAW2 = gls(tbio.allo.c ~ Hage + I(Hage^2), subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.HageC.SAW2 = lme(fixed = tbio.allo.c ~ Hage + I(Hage^2), random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.SAW2, lme.HageC.SAW2)
#no difference in model fit

#use linear model
lm.HageC.SAW2 = lm(tbio.allo.c ~ Hage + I(Hage^2), subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.SAW2)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.SAW = gls(tbio.allo.c ~ POP_Densit, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.POP_DensitC.SAW = lme(fixed = tbio.allo.c ~ POP_Densit, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.SAW, lme.POP_DensitC.SAW)
#no difference in model fit

#use linear model
lm.POP_DensitC.SAW = lm(tbio.allo.c ~ POP_Densit, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.SAW)

##############
#percent silt#
##############

#select random effects
gls.SiltC.SAW = gls(tbio.allo.c ~ Silt, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.SiltC.SAW = lme(fixed = tbio.allo.c ~ Silt, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.SAW, lme.SiltC.SAW)
#no difference in model fit

#use linear model
lm.SiltC.SAW = lm(tbio.allo.c ~ Silt, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.SAW)

###############
#median income#
###############

#select random effects
gls.medINC.1C.SAW = gls(tbio.allo.c ~ medINC.1, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.medINC.1C.SAW = lme(fixed = tbio.allo.c ~ medINC.1, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.SAW, lme.medINC.1C.SAW)
#no difference in model fit

#use linear model
lm.medINC.1C.SAW = lm(tbio.allo.c ~ medINC.1, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.SAW)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.SAW = gls(tbio.allo.c ~ TotalValua, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.TotalValuaC.SAW = lme(fixed = tbio.allo.c ~ TotalValua, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.SAW, lme.TotalValuaC.SAW)
#no difference in model fit

#use linear model
lm.TotalValuaC.SAW = lm(tbio.allo.c ~ TotalValua, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.SAW)

################
#percent vacant#
################

#select random effects
gls.perVACC.SAW = gls(tbio.allo.c ~ perVAC, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.perVACC.SAW = lme(fixed = tbio.allo.c ~ perVAC, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.SAW, lme.perVACC.SAW)
#no difference in model fit

#use linear model
lm.perVACC.SAW = lm(tbio.allo.c ~ perVAC, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.SAW)

##################
#percent minority#
##################

#select random effects
gls.perMINC.SAW = gls(tbio.allo.c ~ perMIN, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.perMINC.SAW = lme(fixed = tbio.allo.c ~ perMIN, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.SAW, lme.perMINC.SAW)
#no difference in model fit

#use linear model
lm.perMINC.SAW = lm(tbio.allo.c ~ perMIN, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.SAW)

#################
#percent married#
#################

#select random effects
gls.perMARC.SAW = gls(tbio.allo.c ~ perMAR, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.perMARC.SAW = lme(fixed = tbio.allo.c ~ perMAR, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.SAW, lme.perMARC.SAW)
#no difference in model fit

#use linear model
lm.perMARC.SAW = lm(tbio.allo.c ~ perMAR, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.SAW)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.SAW = gls(tbio.allo.c ~ medAGE.1, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.medAGE.1C.SAW = lme(fixed = tbio.allo.c ~ medAGE.1, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.SAW, lme.medAGE.1C.SAW)
#no difference in model fit

#use linear model
lm.medAGE.1C.SAW = lm(tbio.allo.c ~ medAGE.1, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.SAW)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.SAW = gls(tbio.allo.c ~ dur, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.durC.SAW = lme(fixed = tbio.allo.c ~ dur, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.SAW, lme.durC.SAW)
#no difference in model fit

#use linear model
lm.durC.SAW = lm(tbio.allo.c ~ dur, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.SAW)

###########
#yard size#
###########

#select random effects
gls.ydC.SAW = gls(tbio.allo.c ~ yd, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.ydC.SAW = lme(fixed = tbio.allo.c ~ yd, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.SAW, lme.ydC.SAW)
#no difference in model fit

#use linear model
lm.ydC.SAW = lm(tbio.allo.c ~ yd, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC.SAW)

#############
#############
#All Biomass#
#############
#############

#############
#housing age#
#############

#select random effects
gls.HageC = gls(tbio.allo.c ~ Hage, data = sitr, na.action = na.omit)
lme.HageC = lme(fixed = tbio.allo.c ~ Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC, lme.HageC)
#no difference in model fit

#use linear model
lm.HageC = lm(tbio.allo.c ~ Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC)

################
#housing age ^2#
################

#select random effects
gls.HageC2 = gls(tbio.allo.c ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.HageC2 = lme(fixed = tbio.allo.c ~ Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC2, lme.HageC2)
#no difference in model fit

#use linear model
lm.HageC2 = lm(tbio.allo.c ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC2)

####################
#population density#
####################

#select random effects
gls.POP_DensitC = gls(tbio.allo.c ~ POP_Densit, data = sitr, na.action = na.omit)
lme.POP_DensitC = lme(fixed = tbio.allo.c ~ POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC, lme.POP_DensitC)
#no difference in model fit

#use linear model
lm.POP_DensitC = lm(tbio.allo.c ~ POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC)

##############
#percent silt#
##############

#select random effects
gls.SiltC = gls(tbio.allo.c ~ Silt, data = sitr, na.action = na.omit)
lme.SiltC = lme(fixed = tbio.allo.c ~ Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC, lme.SiltC)
#no difference in model fit

#use linear model
lm.SiltC = lm(tbio.allo.c ~ Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC)

###############
#median income#
###############

#select random effects
gls.medINC.1C = gls(tbio.allo.c ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C = lme(fixed = tbio.allo.c ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C, lme.medINC.1C)
#no difference in model fit

#use linear model
lm.medINC.1C = lm(tbio.allo.c ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C)

################
#assessed value#
################

#select random effects
gls.TotalValuaC = gls(tbio.allo.c ~ TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuaC = lme(fixed = tbio.allo.c ~ TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC, lme.TotalValuaC)
#no difference in model fit

#use linear model
lm.TotalValuaC = lm(tbio.allo.c ~ TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC)

################
#percent vacant#
################

#select random effects
gls.perVACC = gls(tbio.allo.c ~ perVAC, data = sitr, na.action = na.omit)
lme.perVACC = lme(fixed = tbio.allo.c ~ perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC, lme.perVACC)
#no difference in model fit

#use linear model
lm.perVACC = lm(tbio.allo.c ~ perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC)

##################
#percent minority#
##################

#select random effects
gls.perMINC = gls(tbio.allo.c ~ perMIN, data = sitr, na.action = na.omit)
lme.perMINC = lme(fixed = tbio.allo.c ~ perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC, lme.perMINC)
#no difference in model fit

#use linear model
lm.perMINC = lm(tbio.allo.c ~ perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC)

#################
#percent married#
#################

#select random effects
gls.perMARC = gls(tbio.allo.c ~ perMAR, data = sitr, na.action = na.omit)
lme.perMARC = lme(fixed = tbio.allo.c ~ perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC, lme.perMARC)
#no difference in model fit

#use linear model
lm.perMARC = lm(tbio.allo.c ~ perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC)

###############
#median income#
###############

#select random effects
gls.medINC.1C = gls(tbio.allo.c ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C = lme(fixed = tbio.allo.c ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C, lme.medINC.1C)
#no difference in model fit

#use linear model
lm.medINC.1C = lm(tbio.allo.c ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C = gls(tbio.allo.c ~ medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1C = lme(fixed = tbio.allo.c ~ medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C, lme.medAGE.1C)
#no difference in model fit

#use linear model
lm.medAGE.1C = lm(tbio.allo.c ~ medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C)

##########################
#median resident duration#
##########################

#select random effects
gls.durC = gls(tbio.allo.c ~ dur, data = sitr, na.action = na.omit)
lme.durC = lme(fixed = tbio.allo.c ~ dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC, lme.durC)
#no difference in model fit

#use linear model
lm.durC = lm(tbio.allo.c ~ dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC)

###########
#yard size#
###########

#select random effects
gls.ydC = gls(tbio.allo.c ~ yd, data = sitr, na.action = na.omit)
lme.ydC = lme(fixed = tbio.allo.c ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC, lme.ydC)
#no difference in model fit

#use linear model
lm.ydC = lm(tbio.allo.c ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC)

################################################################################
################################################################################
#Mutiple regressions examining the relative contribution of biogeosociochemical
#variables to overall model fit
################################################################################
################################################################################

#############
#############
#All Biomass#
#############
#############

#by medINC and medAGE

##############
#collinearity#
##############

#fit medAGE as a function of medINC
lm.1 = lm(medAGE.1 ~ medINC.1, data = sitr)
#no significant relationship. Data are not collinear and can be used in the same model

lm.incage = lm(tbio.allo.c ~ medINC.1 + medAGE.1, data = sitr, na.action = na.omit)

lm.incage.1 = lm(tbio.allo.c ~ medINC.1, data = sitr, na.action = na.omit)
lm.incage.2 = lm(tbio.allo.c ~ medAGE.1, data = sitr, na.action = na.omit)
AIC(lm.incage, lm.incage.1, lm.incage.2)

calc.relimp(lm.incage, type = c("lmg"), rela = TRUE)

################################################################################
################################################################################
#Relationship between above- and below-ground C stocks
################################################################################
################################################################################

lm.soiltree = lm(stoC_to50 ~ tbio.allo.c, data = sitr)


####################################################################################
####################################################################################
#Make figures
####################################################################################
####################################################################################
setwd("C:\\Users\\Alix\\Box Sync\\UNH\\Projects\\CCS_Manchester\\Data\\R Projects\\
      carbon-small-cities\\Figures")

##########
#Figure 2#
##########

#call pdf.options to define graphical parameters in pdf export file
#pdf.options(width= 4.5, height= 3.5, paper="letter", pointsize=10)

#name pdf export file
#pdf(file="Figure_2.pdf")

#call jpeg file
jpeg(file = "Figure_2.jpeg", width = 4.5, height = 3.5, units = "in", res = 400, pointsize = 10)

#set graphical parameters
par(mfrow = c (1,1), mar = c(0.2, 0.25, 0.2, 0.2), oma = c(5,5,0.5,5))

#create new column that makes Depth go in reverse order so that 
#the shallowest soils go at the top of the plot when rotated horizontally
sites_scn$DF = as.factor(ifelse(sites_scn$Depth.Fraction == "0 to 10", "E", 
                                ifelse(sites_scn$Depth.Fraction == "10 to 20", "D",
                                      ifelse(sites_scn$Depth.Fraction == "20 to 30", "C", 
                                             ifelse(sites_scn$Depth.Fraction == "30 to 40", "B",
                                                    ifelse(sites_scn$Depth.Fraction == "40 to 50", "A", NA))))))

boxplot(stoC ~ DF, data = sites_scn, horizontal = T,  axes = F,  notch = F, ylim = c(0, 6e4), range = 0,
        col =c(adjustcolor("coral", alpha = 0.2), 
               adjustcolor("coral", alpha = 0.4),
               adjustcolor("coral", alpha = 0.6),
               adjustcolor("coral", alpha = 0.8),
               adjustcolor("coral", alpha = 1.0)))
box(lty = 1)
axis(1, at = c(0, 2e4, 4e4, 6e4), c(0, 2, 4, 6), cex.axis = 1)
axis(2, at = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), lab = c("50", "40", "30", "20", "10", "0"), cex.axis = 1)
mtext(side = 1, expression(paste("Soil C (kg "*m^{-2}*")")), line = 3, outer = T, cex = 1)
mtext(side = 2, "Depth (cm)", line = 3, cex = 1)
text(53e3, 5, "a")
text(53e3, 4, "b")
text(53e3, 3.25, "c")
text(54e3, 2, "cd")
text(53e3, 1, "d")

dev.off()

##########
#Figure 3#
##########

#call pdf.options to define graphical parameters in pdf export file
#pdf.options(width= 3, height= 7, paper="letter", pointsize=12)

#name pdf export file
#pdf(file="Figure_3.pdf")
jpeg(file = "Figure_3.jpeg", width = 3, height = 7, units = "in", res = 400, pointsize = 10)

par(mfrow = c (5,1), mar = c(0.25, 0.25, 0.25, 0.25), oma = c(5, 5, 5, 5))

lm.10 = lm(stoC_to10 ~ Hage, data = sitr)
plot(stoC_to10 ~ Hage, data = sitr, axes = F, pch = 21, col = "black", 
     bg = adjustcolor("coral", alpha = 1.0), cex = 1.5, xlab = " ", ylab = " ", 
     ylim = c(0, 6e4), xlim = c(0,150))
box(lty = 1)
axis(2, at = c(0, 2e4, 4e4, 6e4), c(0, 2, 4, 6), cex.axis = 1)
curve(summary(lm.10)[[4]][2] * x + summary(lm.10)[[4]][1], lwd = 2, lty = 1, add=TRUE)
text(5, 55e3, "(a)", cex = 1.0)
#text(100, 100, "y = 213.89 + 1.37 * x")
text(100, 55e3, expression("p = 0.001," ~r^{2}*" = 0.28"))

lm.20 = lm(stoC_at20 ~ Hage, data = sitr)
plot(stoC_at20 ~ Hage, data = sitr, axes = F, pch = 22, col = "black", 
     bg = adjustcolor("coral", alpha = 0.8), cex = 1.5, xlab = " ", ylab = " ", 
     ylim = c(0, 6e4), xlim = c(0,150))
box(lty = 1)
axis(2, at = c(0, 2e4, 4e4, 6e4), c(0, 2, 4, 6), cex.axis = 1)
curve(summary(lm.20)[[4]][2] * x + summary(lm.20)[[4]][1], lwd = 2, lty = 1, add=TRUE)
text(5, 55e3, "(b)", cex = 1.0)
#text(100, 55e3, "y = 190.85 + 0.87 * x")
text(100, 55e3, expression("p = 0.036," ~r^{2}*" = 0.10"))

lm.30 = lm(stoC_at30 ~ Hage, data = sitr)
plot(stoC_at30 ~ Hage, data = sitr, axes = F, pch = 23, col = "black", 
     bg = adjustcolor("coral", alpha =0.6), cex = 1.5, xlab = " ", ylab = " ", 
     ylim = c(0, 6e4), xlim = c(0,150))
box(lty = 1)
axis(2, at = c(0, 2e4, 4e4, 6e4), c(0, 2, 4, 6), cex.axis = 1)
curve(summary(lm.30)[[4]][2] * x + summary(lm.30)[[4]][1], lwd = 2, lty = 1, add=TRUE)
text(5, 55e3, "(c)", cex = 1.0)
#text(100, 500, "y = 97.21 + 1.41 * x")
text(100, 475e2, expression("p = 0.037," ~r^{2}*" = 0.11"))

lm.40 = lm(stoC_at40 ~ Hage, data = sitr)
plot(stoC_at40 ~ Hage, data = sitr, axes = F, pch = 24, col = "black", 
     bg = adjustcolor("coral", alpha = 0.4), cex = 1.5, xlab = " ", ylab = " ", 
     ylim = c(0, 6e4), xlim = c(0,150))
box(lty = 1)
axis(2, at = c(0, 2e4, 4e4, 6e4), c(0, 2, 4, 6), cex.axis = 1)
curve(summary(lm.40)[[4]][2] * x + summary(lm.40)[[4]][1], lwd = 2, lty = 1, add=TRUE)
text(5, 55e3, "(d)", cex = 1.0)
#text(100, 55e3, "y = 62.09 + 1.71 * x")
text(100, 575e2, expression("p = 0.033" ~r^{2}*" = 0.11"))

lm.50 = lm(stoC_at50 ~ Hage, data = sitr)
plot(stoC_at50 ~ Hage, data = sitr, axes = F, pch = 25, col = "black", 
     bg = adjustcolor("coral", alpha = 0.2), cex = 1.5, xlab = " ", ylab = " ", 
     ylim = c(0, 6e4), xlim = c(0,150))
box(lty = 1)
axis(1, at = c(0, 35, 70, 105, 140), cex.axis = 1)
axis(2, at = c(0, 2e4, 4e4, 6e4), c(0, 2, 4, 6), cex.axis = 1)
curve(summary(lm.50)[[4]][2] * x + summary(lm.50)[[4]][1], lwd = 2, lty = 1, add=TRUE)
text(5, 55e3, "(e)", cex = 1.0)
#text(100, 500, "y = 77.06 + 0.88 * x")
text(100, 55e3, expression("p = 0.067," ~r^{2}*" = 0.13"))
mtext(side = 1, "Housing  Age (years)", line = 3, cex = 0.75)
mtext(side = 2, expression(paste("Soil C (kg "*m^{-2}*")")), line = 2, outer = T, cex = 0.75)

dev.off()

##########
#Figure 4#
##########

#recode size class for plotting

sitrcla$fiac.2 = ifelse(sitrcla$fiac == "SAP", 1, ifelse(sitrcla$fiac == "POL", 2,
                                                         ifelse(sitrcla$fiac == "SAW", 3, NA)))

#call pdf.options to define graphical parameters in pdf export file
#pdf.options(width= 4.5, height= 3.5, paper="letter", pointsize=10)

#name pdf export file
#pdf(file="Figure_4.pdf")
jpeg(file = "Figure_4.jpeg", width = 4.5, height = 3.5, units = "in", res = 400, pointsize = 10)

par(mfrow = c (1,1), mar = c(0.25, 0.25, 0.25, 0.25), oma = c(5,5,0.5,5))

boxplot(tbio.allo.c ~ fiac.2, data = sitrcla, range = 0, axes = F, ylim = c(0,5e4), 
        col = c(adjustcolor("darkolivegreen", alpha = 0.2), 
                adjustcolor("darkolivegreen", alpha = 0.6),
                adjustcolor("darkolivegreen", alpha = 1.0)))
         
box(lty = 1)
axis(1, at = c(1:3), lab = c("Sapling", "Pole Timb", "Saw Timb"))
axis(2, at = c(0, 1e4, 2e4, 3e4, 4e4, 5e4), lab = c(0, 1, 2, 3, 4, 5))
mtext(side = 2, expression(paste("Biomass C (kg "*m^{-2}*")")), line = 2.5, outer = T, cex = 1)
mtext(side = 1, "Size Class", line = 2.5, cex = 1)
text(1, 1e4, "a", cex = 1)
text(2, 1e4, "ab", cex = 1)
text(3, 5e4, "b", cex = 1)

dev.off()

##########
#Figure 5#
##########

#call pdf.options to define graphical parameters in pdf export file
#pdf.options(width= 3, height= 6, paper="letter", pointsize=12)

#name pdf export file
#pdf(file="Figure_5.pdf")
jpeg(file = "Figure_5.jpeg", width = 3, height = 6, units = "in", res = 400, pointsize = 10)

par(mfrow = c (3,1), mar = c(0.25, 0.25, 0.25, 0.25), oma = c(5, 5, 5, 5))

sitrclasub.1 = sitrcla[sitrcla$fiac == "SAP", ]
sitrclasub = sitrclasub.1[complete.cases(sitrclasub.1$tbio.allo.c), ]
sitrclasub = sitrclasub[order(sitrclasub$Hage), ]

lm.SAP = lm(tbio.allo.c ~ Hage, subset = fiac == "SAP", data = sitrcla)
qd.SAP = lm(tbio.allo.c ~ Hage + I(Hage^2), subset = fiac == "SAP", data = sitrcla)

plot(tbio.allo.c ~ Hage, subset = fiac == "SAP", data = sitrcla, axes = F, pch = 21, col = "black", 
     bg = adjustcolor("darkolivegreen", alpha = 0.2), cex = 1.5, xlab = " ", ylab = " ", 
     ylim = c(0,2200), xlim = c(0,150))
box(lty = 1)
axis(2, at = c(0, 0.5e3, 1e3, 1.5e3, 2e3), c("0.00", "0.05", "0.10", "0.15", "0.20"), cex.axis = 1)
curve(predict(lm.SAP,newdata=data.frame(Hage=x)),add=T, lwd = 2, lty = 1)
curve(predict(qd.SAP,newdata=data.frame(Hage=x)),add=T, lwd = 2, lty = 2)
text(5, 2e3, "(a)", cex = 1)
#text(100, 100, "y = 213.89 + 1.37 * x")
text(95, 2e3, expression("lin: p = 0.46," ~r^{2}*" = -0.03"))
text(87, 1.85e3, expression("quad: p = 0.76," ~r^{2}*" = -0.10"))

sitrclasub.1 = sitrcla[sitrcla$fiac == "POL", ]
sitrclasub = sitrclasub.1[complete.cases(sitrclasub.1$tbio.allo.c), ]
sitrclasub = sitrclasub[order(sitrclasub$Hage), ]

lm.POL = lm(tbio.allo.c ~ Hage, subset = fiac == "POL", data = sitrcla)
qd.POL = lm(tbio.allo.c ~ Hage + I(Hage^2), subset = fiac == "POL", data = sitrcla)

plot(tbio.allo.c ~ Hage, subset = fiac == "POL", data = sitrcla, axes = F, pch = 22, col = "black", 
     bg = adjustcolor("darkolivegreen", alpha = 0.6), cex = 1.5, xlab = " ", ylab = " ", 
     ylim = c(0,8950), xlim = c(0,150))
box(lty = 1)
axis(2, at = c(0, 2e3, 4e3, 6e3, 8e3), c("0.0", "0.2", "0.4", "0.6", "0.8"), cex.axis = 1)
curve(predict(lm.POL,newdata=data.frame(Hage=x)),add=T, lwd = 2, lty = 1)
curve(predict(qd.POL,newdata=data.frame(Hage=x)),add=T, lwd = 2, lty = 2)
text(5, 8e3, "(b)", cex = 1)
#text(100, 100, "y = 213.89 + 1.37 * x")
text(95, 8e3, expression("lin: p = 0.35," ~r^{2}*" = 0.00"))
text(87, 7.35e3, expression("quad: p = 0.62," ~r^{2}*" = -0.05"))


sitrclasub.1 = sitrcla[sitrcla$fiac == "SAW", ]
sitrclasub = sitrclasub.1[complete.cases(sitrclasub.1$tbio.allo.c), ]
sitrclasub = sitrclasub[order(sitrclasub$Hage), ]

lm.SAW = lm(tbio.allo.c ~ Hage, subset = fiac == "SAW", data = sitrclasub)
qd.SAW = lm(tbio.allo.c ~ Hage + I(Hage^2), subset = fiac == "SAW", data = sitrclasub)

plot(tbio.allo.c ~ Hage, subset = fiac == "SAW", data = sitrcla, axes = F, pch = 23, col = "black", 
     bg = adjustcolor("darkolivegreen", alpha = 1.0), cex = 1.5, xlab = " ", ylab = " ", 
     ylim = c(0,60000), xlim = c(0,150))# 
box(lty = 1)
axis(2, at = c(0, 2e4, 4e4, 6e4), c(0, 2, 4, 6), cex.axis = 1)
curve(predict(lm.SAW,newdata=data.frame(Hage=x)),add=T, lwd = 2, lty = 1)
curve(predict(qd.SAW,newdata=data.frame(Hage=x)),add=T, lwd = 2, lty = 2)

text(5, 5.85e4, "(c)", cex = 1)
#text(100, 100, "y = 213.89 + 1.37 * x")
text(95, 5.85e4, expression("lin: p = 0.85," ~r^{2}*" = -0.04"))
text(85, 5.45e4, expression("quad: p = 0.06," ~r^{2}*" = 0.16"))
axis(1, at = c(0, 35, 70, 105, 140), cex.axis = 1)
mtext(side = 1, "Housing  Age (years)", line = 3, cex = 0.85)
mtext(side = 2, expression(paste("Biomass C (kg "*m^{-2}*")")), line = 2, outer = T, cex = 0.85)

dev.off()

##########
#Figure 6#
##########

#call pdf.options to define graphical parameters in pdf export file
#pdf.options(width= 5.5, height= 3, paper="letter", pointsize=12)

#name pdf export file
#pdf(file="Figure_6.pdf")
jpeg(file = "Figure_6.jpeg", width = 5.5, height = 3, units = "in", res = 400, pointsize = 10)

par(mfrow = c (1,3), mar = c(0.25, 0.25, 1.25, 0.25), oma = c(5, 5, 4, 0.5))

lm.medINC = lm(tbio.allo.c ~ medINC.1, data = sitr)
plot(tbio.allo.c ~ medINC, data = sitr, axes = F, pch = 21, col = "black", bg = "gray80", cex = 1.5, xlab = " ", ylab = " ",
     ylim = c(0, 5.5e4))
points(tbio.allo.c ~ medINC.1, data = sitr, pch = 21, col = "black", bg = "gold", cex = 1.5, ylim = c(0, 5.5e4))
box(lty = 1)
axis(1, at = c(4e4, 6e4, 8e4, 1e5), lab = c(40, 60, 80, 100), cex.axis = 1)
axis(2, at = c(0, 1e4, 2e4, 3e4, 4e4, 5e4), lab = c(0, 1, 2, 3, 4, 5), cex.axis = 1)
text(35000, 5.25e4, "(a)", cex = 1)
text(85e3, 5.25e4, expression("p = 0.008," ~r^{2}*" = 0.20"))
curve(summary(lm.medINC)[[4]][2] * x + summary(lm.medINC)[[4]][1], lwd = 2, lty = 1, add=TRUE)
mtext(side = 1, "Median Income (1,000$)", line = 2.5, outer = F, cex = 0.75)
mtext(side = 2, expression(paste("Observed Biomass C (kg "*m^{-2}*")")), line = 2.5, outer = F, cex = 0.75)

lm.medAGE = lm(tbio.allo.c ~ medAGE.1, data = sitr)
plot(tbio.allo.c ~ MEDIANAGE, data = sitr, axes = F, pch = 22, col = "black", bg = "gray80", cex = 1.5, xlab = " ", ylab = " ",
     ylim = c(0, 5.5e4), xlim = c(20, 80))
points(tbio.allo.c ~ medAGE.1, data = sitr, pch = 22, col = "black", bg = "dodgerblue", cex = 1.5)
box(lty = 1)
axis(1, at = c(20, 40, 60, 80), cex.axis = 1)
text(22, 5.25e4, "(b)", cex = 1)
curve(summary(lm.medAGE)[[4]][2] * x + summary(lm.medAGE)[[4]][1], lwd = 2, lty = 1, add=TRUE)
text(60, 5.25e4, expression("p = 0.011," ~r^{2}*" = 0.18"))
mtext(side = 1, "Resident Age (years)", line = 2.5, outer = F, cex = 0.75)


sitrsub = sitr[complete.cases(sitr$medAGE.1) & complete.cases(sitr$medINC.1) & complete.cases(sitr$tbio.allo.c), ]
lm.incage = lm(tbio.allo.c ~ medINC.1 + medAGE.1, data = sitrsub, na.action = na.omit)
lm.incall = lm(sitrsub$tbio.allo.c ~ predict(lm.incage))

plot(sitrsub$tbio.allo.c ~ predict(lm.incage), axes = F, pch = 23, col = "black", bg = "darkolivegreen", 
     xlab = " ", ylab = " ", xlim = c(0, 5.5e4), ylim = c(0, 5.5e4), cex = 1.5)
box(lty = 1)     
axis(1, at = c(0, 1e4, 2e4, 3e4, 4e4, 5e4), lab = c(0, 1, 2, 3, 4, 5), cex.axis = 1)
text(1e3, 5.25e4, "(c)", cex = 1)

curve(summary(lm.incall)[[4]][2] * x  + summary(lm.incall)[[4]][1], lwd = 2, lty = 1, add=TRUE)
text(25e3, 5.25e4, expression("p = 0.008," ~r^{2}*" = 0.27"))
mtext(side = 1, expression(paste("Predicted Biomass C (kg "*m^{-2}*")")), line = 2.75, outer = F, cex = 0.75)

dev.off()

#############
#SI Figure 1#
#############

lm.ydSAW = lm(tbio.allo.c ~ yd, data = sitrcla, subset = sitrcla$fiac == "SAW")
lm.ydSAWsub = lm(tbio.allo.c ~ yd, data = sitrcla, subset = sitrcla$fiac == "SAW" & sitrcla$yd < 0.3)

#call pdf.options to define graphical parameters in pdf export file
#pdf.options(width= 4.5, height= 3.5, paper="letter", pointsize=10)

#name pdf export file
#pdf(file="SI_Figure_1.pdf")
jpeg(file = "SI_Figure_1.jpeg", width = 4.5, height = 3.5, units = "in", res = 400, pointsize = 10)

par(mfrow = c (1,1), mar = c(5,5,0.5,5))


plot(tbio.allo.c ~ yd, data = sitrcla, subset = sitrcla$fiac == "SAW", ylim = c(0,60000), yaxt = "n",
     pch = 21, bg = "gray80", cex = 1.5, xlab = "Yard Size (ha)",
     ylab = expression(paste("Biomass C (kg "*m^{-2}*")")))
points(tbio.allo.c ~ yd, data = sitrcla, subset = sitrcla$fiac == "SAW" & sitrcla$yd < 0.3,
       pch = 21,bg = "darkolivegreen", cex = 1.5)
axis(2, at = c(0, 2e4, 4e4, 6e4), c(0, 2, 4, 6), cex.axis = 1)

curve(predict(lm.ydSAW,newdata=data.frame(yd=x)),add=T, lwd = 2, lty = 1)
curve(predict(lm.ydSAWsub,newdata=data.frame(yd=x)),add=T, lwd = 2, lty = 2)

text(0.27, 5.85e4, expression("all yards: p = 0.04," ~r^{2}*" = 0.13"))
text(0.25, 5.44e4, expression("yards < 0.3 ha: p = 0.75," ~r^{2}*" = -0.04"))


dev.off()

