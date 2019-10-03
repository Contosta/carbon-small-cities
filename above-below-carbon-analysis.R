##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
#This script analyzes patterns in above- and below-ground carbon stocks within Manchester, NH, USA
#as part of a paper submitted to Lanscape and Urban Planning (doi:##################)

#The datasets required to run this code are located in the carbon-small-cities repository and include
#xyz datasets

#Code was developed by A. Contosta

####################################################################################
####################################################################################
#Initial set up
####################################################################################
####################################################################################

#call libraries
#library(data.table)
#library(splitstackshape)
#library(zoo)
#library(matrixStats)
#library(MASS)
#library(stringr)
#library(dunn.test)
#library(plyr)
#library(gmodels)
library(nlme)
#library(relaimpo)
library(multcomp)

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

#################################
#Outliers in predictor variables#
#################################

#population density
boxplot(sitr$POP_Densit, ylab = "Population Density")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$POP_Densit,  xlab = "Population Density",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$POP_Densit,  xlab = "Population Density",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)


#housing age
boxplot(sitr$Hage, ylab = "Housing Age")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$Hage,  xlab = "Housing Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$Hage,  xlab = "Housing Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent silt
boxplot(sitr$Silt, ylab = "Silt")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$Silt,  xlab = "Silt",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$Silt,  xlab = "Silt",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent hardwood biomass
boxplot(sitr$perHbio, ylab = "hardwood biomass")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$perHbio,  xlab = "hardwood biomass",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$perHbio,  xlab = "hardwood biomass",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#median income
boxplot(sitr$medINC, ylab = "Median Income")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$medINC,  xlab = "Median Income",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$medINC,  xlab = "Median Income",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#total assessed value
boxplot(sitr$TotalValua, ylab = "Assessed Value")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$TotalValua,  xlab = "Assessed Value",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$TotalValua,  xlab = "Assessed Value",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent vacant housing within census block
boxplot(sitr$perVAC, ylab = "Percent Vacant")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$perVAC,  xlab = "Percent Vacant",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$perVAC,  xlab = "Percent Vacant",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent married couples
boxplot(sitr$perMAR, ylab = "Perecent Married")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$perMAR,  xlab = "Percent Married",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$perMAR,  xlab = "Percent Married",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#residence duration
boxplot(sitr$dur, ylab = "Residence Duration")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$dur,  xlab = "Residence Duration",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$dur,  xlab = "Residence Duration",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)


#median resident age
boxplot(sitr$MEDIANAGE, ylab = "Median Age")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$MEDIANAGE,  xlab = "Median Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$Cseqha), ]
dotchart(sitr$MEDIANAGE,  xlab = "Median Age",
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

#make new columns for aboveground biomass (tbio.allo.c), percent hardwood biomass
#median income (medINC.1), and median age
#(medAGE.1) to remove outliers identified during preliminary data analysis
#outliers include median income < 40K > 100K, age > 60 as well as 
#extremely high biomass values from two of the sites

sitr$perHbio.1 = ifelse(sitr$perHbio > 50000, NA, sitr$perHbio)
sitr$tbio.allo.c = ifelse(sitr$ID == "3" | sitr$ID == "35", NA, sitr$Cseqha)
sitr$medINC.1 = ifelse(sitr$medINC < 4e4 | sitr$medINC > 1e5, NA, sitr$medINC)
sitr$medAGE.1 = ifelse(sitr$MEDIANAGE > 60, NA, sitr$MEDIANAGE)

sitrcla$perHbio.1 = ifelse(sitrcla$perHbio > 50000, NA, sitrcla$perHbio)
sitrcla$tbio.allo.c = ifelse(sitrcla$ID == "3" | sitrcla$ID == "35", NA, sitrcla$Cseqha)
sitrcla$medINC.1 = ifelse(sitrcla$medINC < 4e4 | sitrcla$medINC > 1e5, NA, sitrcla$medINC)
sitrcla$medAGE.1 = ifelse(sitrcla$MEDIANAGE > 60, NA, sitrcla$MEDIANAGE)

sitrloc$perHbio.1 = ifelse(sitrloc$perHbio > 50000, NA, sitrloc$perHbio)
sitrloc$tbio.allo.c = ifelse(sitrloc$Cseqha > 100000, NA, sitrloc$Cseqha)
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
hist(sitr$tbio.allo.c)

#aboveground biomass carbon as a function of size class
hist(sitrcla$tbio.allo.c)

#aboveground biomass carbon as a function of location within yard
hist(sitrloc$tbio.allo.c)

#belowground soil carbon data mostly follow normal distribution
#aboveground biomass carbon data do not and will likely need log transformation

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
gls.treecla = gls(log(tbio.allo.c) ~ fiac, data = sitrcla, na.action = na.omit)
lme.treecla = lme(fixed = log(tbio.allo.c) ~ fiac, random = ~1|ID, data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.treecla, lme.treecla)
#model fit improved with ID as a random intercept

#select variance structures
var.treecla = update(lme.treecla, weights = varIdent(form = ~ 1 | fiac))

#compare model fits
BIC(lme.treecla, var.treecla)
#no difference in model fit

#examine significance of fixed effects
anova(lme.treecla, type = "marginal")
#effect of depth fraction highly significant

#validate model by examining homogeneity and normality of residuals
plot(lme.treecla)
qqnorm(lme.treecla)

#obtain pairwise comparisons of differences between depth fractions
summary((glht(lme.treecla, linfct = mcp(fiac = "Tukey"))))

################################################################################
################################################################################
#Regression-type models for C stocks as a function of biogeochemical and 
#socioeconomic variables across depths
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
gls.perHbio.1C.10 = gls(stoC_at10 ~ perHbio.1, data = sitr, na.action = na.omit)
lme.perHbio.1C.10 = lme(fixed = stoC_at10 ~ perHbio.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perHbio.1C.10, lme.perHbio.1C.10)
#no difference in model fit

#use linear model
lm.perHbio.1C.10 = lm(stoC_at10 ~ perHbio.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perHbio.1C.10)

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
#10 to 20 cm#
#############

lm.HageC.20 = lm(stoC_at20 ~ Hage, data = sitr, na.action = na.omit)
lm.HageC.202 = lm(stoC_at20 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lm.POPC.20 =  lm(stoC_at20 ~ POP_Densit, data = sitr, na.action = na.omit)
lm.SiltC.20 =  lm(stoC_at20 ~ Silt, data = sitr, na.action = na.omit)
lm.perHbioC.20 =  lm(stoC_at20 ~ perHbio, data = sitr, na.action = na.omit)
lm.medINCC.20 =  lm(stoC_at20 ~ medINC.1, data = sitr, na.action = na.omit)
lm.TotalValuaC.20 = lm(stoC_at20 ~ TotalValua, data = sitr, na.action = na.omit)
lm.perVACC.20 =  lm(stoC_at20 ~ perVAC, data = sitr, na.action = na.omit)
lm.perMINC.20 =  lm(stoC_at20 ~ perMIN, data = sitr, na.action = na.omit)
lm.perMARC.20 =  lm(stoC_at20 ~ perMAR, data = sitr, na.action = na.omit)
lm.medAGEC.20 =  lm(stoC_at20 ~ medAGE.1, data = sitr, na.action = na.omit)
lm.durC.20 =  lm(stoC_at20 ~ dur, data = sitr, na.action = na.omit)
lm.yd.20 = lm(stoC_at20 ~ yd, data = sitr, na.action = na.omit)


#############
#20 to 30 cm#
#############

lm.HageC.30 = lm(stoC_at30 ~ Hage, data = sitr, na.action = na.omit)
lm.HageC.302 = lm(stoC_at30 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lm.POPC.30 =  lm(stoC_at30 ~ POP_Densit, data = sitr, na.action = na.omit)
lm.SiltC.30 =  lm(stoC_at30 ~ Silt, data = sitr, na.action = na.omit)
lm.perHbioC.30 =  lm(stoC_at30 ~ perHbio, data = sitr, na.action = na.omit)
lm.medINCC.30 =  lm(stoC_at30 ~ medINC.1, data = sitr, na.action = na.omit)
lm.TotalValuaC.30 = lm(stoC_at30 ~ TotalValua, data = sitr, na.action = na.omit)
lm.perVACC.30 =  lm(stoC_at30 ~ perVAC, data = sitr, na.action = na.omit)
lm.perMINC.30 =  lm(stoC_at30 ~ perMIN, data = sitr, na.action = na.omit)
lm.perMARC.30 =  lm(stoC_at30 ~ perMAR, data = sitr, na.action = na.omit)
lm.medAGEC.30 =  lm(stoC_at30 ~ medAGE.1, data = sitr, na.action = na.omit)
lm.durC.30 =  lm(stoC_at30 ~ dur, data = sitr, na.action = na.omit)
lm.yd.30 = lm(stoC_at30 ~ yd, data = sitr, na.action = na.omit)

#############
#30 to 40 cm#
#############

lm.HageC.40 = lm(stoC_at40 ~ Hage, data = sitr, na.action = na.omit)
lm.HageC.402 = lm(stoC_at40 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lm.POPC.40 =  lm(stoC_at40 ~ POP_Densit, data = sitr, na.action = na.omit)
lm.SiltC.40 =  lm(stoC_at40 ~ Silt, data = sitr, na.action = na.omit)
lm.perHbioC.40 =  lm(stoC_at40 ~ perHbio, data = sitr, na.action = na.omit)
lm.medINCC.40 =  lm(stoC_at40 ~ medINC.1, data = sitr, na.action = na.omit)
lm.TotalValuaC.40 = lm(stoC_at40 ~ TotalValua, data = sitr, na.action = na.omit)
lm.perVACC.40 =  lm(stoC_at40 ~ perVAC, data = sitr, na.action = na.omit)
lm.perMINC.40 =  lm(stoC_at40 ~ perMIN, data = sitr, na.action = na.omit)
lm.perMARC.40 =  lm(stoC_at40 ~ perMAR, data = sitr, na.action = na.omit)
lm.medAGEC.40 =  lm(stoC_at40 ~ medAGE.1, data = sitr, na.action = na.omit)
lm.durC.40 =  lm(stoC_at40 ~ dur, data = sitr, na.action = na.omit)
lm.yd.40 = lm(stoC_at40 ~ yd, data = sitr, na.action = na.omit)

#############
#40 to 50 cm#
#############

lm.HageC.50 = lm(stoC_at50 ~ Hage, data = sitr, na.action = na.omit)
lm.HageC.502 = lm(stoC_at50 ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lm.POPC.50 =  lm(stoC_at50 ~ POP_Densit, data = sitr, na.action = na.omit)
lm.SiltC.50 =  lm(stoC_at50 ~ Silt, data = sitr, na.action = na.omit)
lm.perHbioC.50 =  lm(stoC_at50 ~ perHbio, data = sitr, na.action = na.omit)
lm.medINCC.50 =  lm(stoC_at50 ~ medINC.1, data = sitr, na.action = na.omit)
lm.TotalValuaC.50 = lm(stoC_at50 ~ TotalValua, data = sitr, na.action = na.omit)
lm.perVACC.50 =  lm(stoC_at50 ~ perVAC, data = sitr, na.action = na.omit)
lm.perMINC.50 =  lm(stoC_at50 ~ perMIN, data = sitr, na.action = na.omit)
lm.perMARC.50 =  lm(stoC_at50 ~ perMAR, data = sitr, na.action = na.omit)
lm.medAGEC.50 =  lm(stoC_at50 ~ medAGE.1, data = sitr, na.action = na.omit)
lm.durC.50 =  lm(stoC_at50 ~ dur, data = sitr, na.action = na.omit)
lm.yd.50 = lm(stoC_at50 ~ yd, data = sitr, na.action = na.omit)
