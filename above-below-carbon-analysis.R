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
hist(sitr$tbio.allo.c)

#aboveground biomass carbon as a function of size class
hist(sitrcla$tbio.allo.c)

#aboveground biomass carbon as a function of location within yard
hist(sitrloc$tbio.allo.c)

#yard size and proportion of parcel comprised of yard
hist(sitr$yd)
hist(sitr$peryd)

#belowground soil carbon data mostly follow normal distribution
#aboveground biomass carbon data do not and will likely need log transformation
#yard size and percent of parcel comprised of yard do not follow a normal distribution
#and will need to be log transformed when used as response variables

sitr$ln.Cseq = ifelse(sitr$Cseqha == 0, 0, log(sitr$Cseqha))
sitrcla$ln.Cseq = ifelse(sitrcla$Cseqha == 0, 0, log(sitrcla$Cseqha))
sitrloc$ln.Cseq = ifelse(sitrloc$Cseqha == 0, 0, log(sitrloc$Cseqha))

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
boxplot(sitr$ln.Cseq)

#aboveground biomass carbon, within size classes
boxplot(sitrcla[sitrcla$fiac == "POL", ]$ln.Cseq)
boxplot(sitrcla[sitrcla$fiac == "SAP", ]$ln.Cseq)
boxplot(sitrcla[sitrcla$fiac == "SAW", ]$ln.Cseq)

#aboveground biomass carbon, all size classes, separated by yard location
boxplot(sitrloc$ln.Cseq)

#################################
#Outliers in predictor variables#
#################################

#population density
boxplot(sitr$POP_Densit, ylab = "Population Density")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$POP_Densit,  xlab = "Population Density",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$POP_Densit,  xlab = "Population Density",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)


#housing age
boxplot(sitr$Hage, ylab = "Housing Age")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$Hage,  xlab = "Housing Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$Hage,  xlab = "Housing Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent silt
boxplot(sitr$Silt, ylab = "Silt")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$Silt,  xlab = "Silt",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$Silt,  xlab = "Silt",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent hardwood biomass
boxplot(sitr$ln.perH, ylab = "hardwood biomass")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$ln.perH,  xlab = "hardwood biomass",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$ln.perH,  xlab = "hardwood biomass",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#median income
boxplot(sitr$medINC, ylab = "Median Income")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$medINC,  xlab = "Median Income",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$medINC,  xlab = "Median Income",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#total assessed value
boxplot(sitr$TotalValua, ylab = "Assessed Value")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$TotalValua,  xlab = "Assessed Value",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$TotalValua,  xlab = "Assessed Value",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent vacant housing within census block
boxplot(sitr$perVAC, ylab = "Percent Vacant")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$perVAC,  xlab = "Percent Vacant",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$perVAC,  xlab = "Percent Vacant",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#percent married couples
boxplot(sitr$perMAR, ylab = "Perecent Married")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$perMAR,  xlab = "Percent Married",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$perMAR,  xlab = "Percent Married",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#residence duration
boxplot(sitr$dur, ylab = "Residence Duration")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$dur,  xlab = "Residence Duration",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$dur,  xlab = "Residence Duration",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#median resident age
boxplot(sitr$MEDIANAGE, ylab = "Median Age")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$MEDIANAGE,  xlab = "Median Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
dotchart(sitr$MEDIANAGE,  xlab = "Median Age",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)

#yard size
boxplot(sitr$yd, ylab = "Yard Size")
sitr  <- sitr[order(sitr$stoC_at10), ]
dotchart(sitr$yd,  xlab = "Yard Size",
         ylab = "Order of the data", cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
sitr  <- sitr[order(sitr$ln.Cseq), ]
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


#make new columns for median income (medINC.1), and median age
#(medAGE.1) to remove outliers identified during preliminary data analysis
#outliers include median income < 40K > 100K, age > 60 

sitr$medINC.1 = ifelse(sitr$medINC < 4e4 | sitr$medINC > 1e5, NA, sitr$medINC)
sitr$medAGE.1 = ifelse(sitr$MEDIANAGE > 60, NA, sitr$MEDIANAGE)

sitrcla$medINC.1 = ifelse(sitrcla$medINC < 4e4 | sitrcla$medINC > 1e5, NA, sitrcla$medINC)
sitrcla$medAGE.1 = ifelse(sitrcla$MEDIANAGE > 60, NA, sitrcla$MEDIANAGE)

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
boxplot(ln.Cseq ~ fiac, data = sitrcla)

#aboveground biomass carbon as a function of location in yard
boxplot(ln.Cseq ~ Loc, data = sitrloc)

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
gls.treecla = gls(ln.Cseq ~ fiac, data = sitrcla, na.action = na.omit)
lme.treecla = lme(fixed = ln.Cseq ~ fiac, random = ~1|ID, data = sitrcla, na.action = na.omit)

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
gls.HageC.SAP = gls(ln.Cseq ~ Hage, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.HageC.SAP = lme(fixed = ln.Cseq ~ Hage, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.SAP, lme.HageC.SAP)
#no difference in model fit

#use linear model
lm.HageC.SAP = lm(ln.Cseq ~ Hage, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.SAP)

################
#housing age ^2#
################

#select random effects
gls.HageC.SAP2 = gls(ln.Cseq ~ Hage + I(Hage^2), subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.HageC.SAP2 = lme(fixed = ln.Cseq ~ Hage + I(Hage^2), random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.SAP2, lme.HageC.SAP2)
#no difference in model fit

#use linear model
lm.HageC.SAP2 = lm(ln.Cseq ~ Hage + I(Hage^2), subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.SAP2)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.SAP = gls(ln.Cseq ~ POP_Densit, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.POP_DensitC.SAP = lme(fixed = ln.Cseq ~ POP_Densit, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.SAP, lme.POP_DensitC.SAP)
#no difference in model fit

#use linear model
lm.POP_DensitC.SAP = lm(ln.Cseq ~ POP_Densit, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.SAP)

##############
#percent silt#
##############

#select random effects
gls.SiltC.SAP = gls(ln.Cseq ~ Silt, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.SiltC.SAP = lme(fixed = ln.Cseq ~ Silt, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.SAP, lme.SiltC.SAP)
#no difference in model fit

#use linear model
lm.SiltC.SAP = lm(ln.Cseq ~ Silt, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.SAP)

###############
#median income#
###############

#select random effects
gls.medINC.1C.SAP = gls(ln.Cseq ~ medINC.1, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.medINC.1C.SAP = lme(fixed = ln.Cseq ~ medINC.1, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.SAP, lme.medINC.1C.SAP)
#no difference in model fit

#use linear model
lm.medINC.1C.SAP = lm(ln.Cseq ~ medINC.1, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.SAP)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.SAP = gls(ln.Cseq ~ TotalValua, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.TotalValuaC.SAP = lme(fixed = ln.Cseq ~ TotalValua, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.SAP, lme.TotalValuaC.SAP)
#no difference in model fit

#use linear model
lm.TotalValuaC.SAP = lm(ln.Cseq ~ TotalValua, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.SAP)

################
#percent vacant#
################

#select random effects
gls.perVACC.SAP = gls(ln.Cseq ~ perVAC, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.perVACC.SAP = lme(fixed = ln.Cseq ~ perVAC, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.SAP, lme.perVACC.SAP)
#no difference in model fit

#use linear model
lm.perVACC.SAP = lm(ln.Cseq ~ perVAC, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.SAP)

##################
#percent minority#
##################

#select random effects
gls.perMINC.SAP = gls(ln.Cseq ~ perMIN, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.perMINC.SAP = lme(fixed = ln.Cseq ~ perMIN, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.SAP, lme.perMINC.SAP)
#no difference in model fit

#use linear model
lm.perMINC.SAP = lm(ln.Cseq ~ perMIN, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.SAP)

#################
#percent married#
#################

#select random effects
gls.perMARC.SAP = gls(ln.Cseq ~ perMAR, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.perMARC.SAP = lme(fixed = ln.Cseq ~ perMAR, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.SAP, lme.perMARC.SAP)
#no difference in model fit

#use linear model
lm.perMARC.SAP = lm(ln.Cseq ~ perMAR, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.SAP)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.SAP = gls(ln.Cseq ~ medAGE.1, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.medAGE.1C.SAP = lme(fixed = ln.Cseq ~ medAGE.1, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.SAP, lme.medAGE.1C.SAP)
#no difference in model fit

#use linear model
lm.medAGE.1C.SAP = lm(ln.Cseq ~ medAGE.1, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.SAP)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.SAP = gls(ln.Cseq ~ dur, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.durC.SAP = lme(fixed = ln.Cseq ~ dur, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.SAP, lme.durC.SAP)
#no difference in model fit

#use linear model
lm.durC.SAP = lm(ln.Cseq ~ dur, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.SAP)

###########
#yard size#
###########

#select random effects
gls.ydC.SAP = gls(ln.Cseq ~ yd, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)
lme.ydC.SAP = lme(fixed = ln.Cseq ~ yd, random = ~1|ID, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.SAP, lme.ydC.SAP)
#no difference in model fit

#use linear model
lm.ydC.SAP = lm(ln.Cseq ~ yd, subset = sitrcla$fiac == "SAP",  data = sitrcla, na.action = na.omit)

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
gls.HageC.POL = gls(ln.Cseq ~ Hage, subset = sitrcla$fiac == "POL", data = sitrcla, na.action = na.omit)
lme.HageC.POL = lme(fixed = ln.Cseq ~ Hage, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.POL, lme.HageC.POL)
#no difference in model fit

#use linear model
lm.HageC.POL = lm(ln.Cseq ~ Hage, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.POL)

################
#housing age ^2#
################

#select random effects
gls.HageC.POL2 = gls(ln.Cseq ~ Hage + I(Hage^2), subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.HageC.POL2 = lme(fixed = ln.Cseq ~ Hage + I(Hage^2), random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.POL2, lme.HageC.POL2)
#no difference in model fit

#use linear model
lm.HageC.POL2 = lm(ln.Cseq ~ Hage + I(Hage^2), subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.POL2)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.POL = gls(ln.Cseq ~ POP_Densit, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.POP_DensitC.POL = lme(fixed = ln.Cseq ~ POP_Densit, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.POL, lme.POP_DensitC.POL)
#no difference in model fit

#use linear model
lm.POP_DensitC.POL = lm(ln.Cseq ~ POP_Densit, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.POL)

##############
#percent silt#
##############

#select random effects
gls.SiltC.POL = gls(ln.Cseq ~ Silt, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.SiltC.POL = lme(fixed = ln.Cseq ~ Silt, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.POL, lme.SiltC.POL)
#no difference in model fit

#use linear model
lm.SiltC.POL = lm(ln.Cseq ~ Silt, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.POL)

###############
#median income#
###############

#select random effects
gls.medINC.1C.POL = gls(ln.Cseq ~ medINC.1, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.medINC.1C.POL = lme(fixed = ln.Cseq ~ medINC.1, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.POL, lme.medINC.1C.POL)
#no difference in model fit

#use linear model
lm.medINC.1C.POL = lm(ln.Cseq ~ medINC.1, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.POL)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.POL = gls(ln.Cseq ~ TotalValua, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.TotalValuaC.POL = lme(fixed = ln.Cseq ~ TotalValua, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.POL, lme.TotalValuaC.POL)
#no difference in model fit

#use linear model
lm.TotalValuaC.POL = lm(ln.Cseq ~ TotalValua, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.POL)

################
#percent vacant#
################

#select random effects
gls.perVACC.POL = gls(ln.Cseq ~ perVAC, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.perVACC.POL = lme(fixed = ln.Cseq ~ perVAC, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.POL, lme.perVACC.POL)
#no difference in model fit

#use linear model
lm.perVACC.POL = lm(ln.Cseq ~ perVAC, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.POL)

##################
#percent minority#
##################

#select random effects
gls.perMINC.POL = gls(ln.Cseq ~ perMIN, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.perMINC.POL = lme(fixed = ln.Cseq ~ perMIN, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.POL, lme.perMINC.POL)
#no difference in model fit

#use linear model
lm.perMINC.POL = lm(ln.Cseq ~ perMIN, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.POL)

#################
#percent married#
#################

#select random effects
gls.perMARC.POL = gls(ln.Cseq ~ perMAR, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.perMARC.POL = lme(fixed = ln.Cseq ~ perMAR, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.POL, lme.perMARC.POL)
#no difference in model fit

#use linear model
lm.perMARC.POL = lm(ln.Cseq ~ perMAR, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.POL)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.POL = gls(ln.Cseq ~ medAGE.1, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.medAGE.1C.POL = lme(fixed = ln.Cseq ~ medAGE.1, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.POL, lme.medAGE.1C.POL)
#no difference in model fit

#use linear model
lm.medAGE.1C.POL = lm(ln.Cseq ~ medAGE.1, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.POL)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.POL = gls(ln.Cseq ~ dur, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.durC.POL = lme(fixed = ln.Cseq ~ dur, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.POL, lme.durC.POL)
#no difference in model fit

#use linear model
lm.durC.POL = lm(ln.Cseq ~ dur, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.POL)

###########
#yard size#
###########

#select random effects
gls.ydC.POL = gls(ln.Cseq ~ yd, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)
lme.ydC.POL = lme(fixed = ln.Cseq ~ yd, random = ~1|ID, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.POL, lme.ydC.POL)
#no difference in model fit

#use linear model
lm.ydC.POL = lm(ln.Cseq ~ yd, subset = sitrcla$fiac == "POL",  data = sitrcla, na.action = na.omit)

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
gls.HageC.SAW = gls(ln.Cseq ~ Hage, subset = sitrcla$fiac == "SAW", data = sitrcla, na.action = na.omit)
lme.HageC.SAW = lme(fixed = ln.Cseq ~ Hage, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.SAW, lme.HageC.SAW)
#no difference in model fit

#use linear model
lm.HageC.SAW = lm(ln.Cseq ~ Hage, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.SAW)

################
#housing age ^2#
################

#select random effects
gls.HageC.SAW2 = gls(ln.Cseq ~ Hage + I(Hage^2), subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.HageC.SAW2 = lme(fixed = ln.Cseq ~ Hage + I(Hage^2), random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC.SAW2, lme.HageC.SAW2)
#no difference in model fit

#use linear model
lm.HageC.SAW2 = lm(ln.Cseq ~ Hage + I(Hage^2), subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC.SAW2)

####################
#population density#
####################

#select random effects
gls.POP_DensitC.SAW = gls(ln.Cseq ~ POP_Densit, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.POP_DensitC.SAW = lme(fixed = ln.Cseq ~ POP_Densit, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC.SAW, lme.POP_DensitC.SAW)
#no difference in model fit

#use linear model
lm.POP_DensitC.SAW = lm(ln.Cseq ~ POP_Densit, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC.SAW)

##############
#percent silt#
##############

#select random effects
gls.SiltC.SAW = gls(ln.Cseq ~ Silt, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.SiltC.SAW = lme(fixed = ln.Cseq ~ Silt, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC.SAW, lme.SiltC.SAW)
#no difference in model fit

#use linear model
lm.SiltC.SAW = lm(ln.Cseq ~ Silt, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC.SAW)

###############
#median income#
###############

#select random effects
gls.medINC.1C.SAW = gls(ln.Cseq ~ medINC.1, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.medINC.1C.SAW = lme(fixed = ln.Cseq ~ medINC.1, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C.SAW, lme.medINC.1C.SAW)
#no difference in model fit

#use linear model
lm.medINC.1C.SAW = lm(ln.Cseq ~ medINC.1, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C.SAW)

################
#assessed value#
################

#select random effects
gls.TotalValuaC.SAW = gls(ln.Cseq ~ TotalValua, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.TotalValuaC.SAW = lme(fixed = ln.Cseq ~ TotalValua, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC.SAW, lme.TotalValuaC.SAW)
#no difference in model fit

#use linear model
lm.TotalValuaC.SAW = lm(ln.Cseq ~ TotalValua, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC.SAW)

################
#percent vacant#
################

#select random effects
gls.perVACC.SAW = gls(ln.Cseq ~ perVAC, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.perVACC.SAW = lme(fixed = ln.Cseq ~ perVAC, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC.SAW, lme.perVACC.SAW)
#no difference in model fit

#use linear model
lm.perVACC.SAW = lm(ln.Cseq ~ perVAC, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC.SAW)

##################
#percent minority#
##################

#select random effects
gls.perMINC.SAW = gls(ln.Cseq ~ perMIN, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.perMINC.SAW = lme(fixed = ln.Cseq ~ perMIN, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC.SAW, lme.perMINC.SAW)
#no difference in model fit

#use linear model
lm.perMINC.SAW = lm(ln.Cseq ~ perMIN, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC.SAW)

#################
#percent married#
#################

#select random effects
gls.perMARC.SAW = gls(ln.Cseq ~ perMAR, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.perMARC.SAW = lme(fixed = ln.Cseq ~ perMAR, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC.SAW, lme.perMARC.SAW)
#no difference in model fit

#use linear model
lm.perMARC.SAW = lm(ln.Cseq ~ perMAR, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC.SAW)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C.SAW = gls(ln.Cseq ~ medAGE.1, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.medAGE.1C.SAW = lme(fixed = ln.Cseq ~ medAGE.1, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C.SAW, lme.medAGE.1C.SAW)
#no difference in model fit

#use linear model
lm.medAGE.1C.SAW = lm(ln.Cseq ~ medAGE.1, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C.SAW)

##########################
#median resident duration#
##########################

#select random effects
gls.durC.SAW = gls(ln.Cseq ~ dur, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.durC.SAW = lme(fixed = ln.Cseq ~ dur, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC.SAW, lme.durC.SAW)
#no difference in model fit

#use linear model
lm.durC.SAW = lm(ln.Cseq ~ dur, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC.SAW)

###########
#yard size#
###########

#select random effects
gls.ydC.SAW = gls(ln.Cseq ~ yd, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)
lme.ydC.SAW = lme(fixed = ln.Cseq ~ yd, random = ~1|ID, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC.SAW, lme.ydC.SAW)
#no difference in model fit

#use linear model
lm.ydC.SAW = lm(ln.Cseq ~ yd, subset = sitrcla$fiac == "SAW",  data = sitrcla, na.action = na.omit)

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
gls.HageC = gls(ln.Cseq ~ Hage, data = sitr, na.action = na.omit)
lme.HageC = lme(fixed = ln.Cseq ~ Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC, lme.HageC)
#no difference in model fit

#use linear model
lm.HageC = lm(ln.Cseq ~ Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC)

################
#housing age ^2#
################

#select random effects
gls.HageC2 = gls(ln.Cseq ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.HageC2 = lme(fixed = ln.Cseq ~ Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC2, lme.HageC2)
#no difference in model fit

#use linear model
lm.HageC2 = lm(ln.Cseq ~ Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.HageC2)

####################
#population density#
####################

#select random effects
gls.POP_DensitC = gls(ln.Cseq ~ POP_Densit, data = sitr, na.action = na.omit)
lme.POP_DensitC = lme(fixed = ln.Cseq ~ POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC, lme.POP_DensitC)
#no difference in model fit

#use linear model
lm.POP_DensitC = lm(ln.Cseq ~ POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC)

##############
#percent silt#
##############

#select random effects
gls.SiltC = gls(ln.Cseq ~ Silt, data = sitr, na.action = na.omit)
lme.SiltC = lme(fixed = ln.Cseq ~ Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC, lme.SiltC)
#no difference in model fit

#use linear model
lm.SiltC = lm(ln.Cseq ~ Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC)

###############
#median income#
###############

#select random effects
gls.medINC.1C = gls(ln.Cseq ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C = lme(fixed = ln.Cseq ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C, lme.medINC.1C)
#no difference in model fit

#use linear model
lm.medINC.1C = lm(ln.Cseq ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C)

################
#assessed value#
################

#select random effects
gls.TotalValuaC = gls(ln.Cseq ~ TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuaC = lme(fixed = ln.Cseq ~ TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC, lme.TotalValuaC)
#no difference in model fit

#use linear model
lm.TotalValuaC = lm(ln.Cseq ~ TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC)

################
#percent vacant#
################

#select random effects
gls.perVACC = gls(ln.Cseq ~ perVAC, data = sitr, na.action = na.omit)
lme.perVACC = lme(fixed = ln.Cseq ~ perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC, lme.perVACC)
#no difference in model fit

#use linear model
lm.perVACC = lm(ln.Cseq ~ perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC)

##################
#percent minority#
##################

#select random effects
gls.perMINC = gls(ln.Cseq ~ perMIN, data = sitr, na.action = na.omit)
lme.perMINC = lme(fixed = ln.Cseq ~ perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC, lme.perMINC)
#no difference in model fit

#use linear model
lm.perMINC = lm(ln.Cseq ~ perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC)

#################
#percent married#
#################

#select random effects
gls.perMARC = gls(ln.Cseq ~ perMAR, data = sitr, na.action = na.omit)
lme.perMARC = lme(fixed = ln.Cseq ~ perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC, lme.perMARC)
#no difference in model fit

#use linear model
lm.perMARC = lm(ln.Cseq ~ perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC)

###############
#median income#
###############

#select random effects
gls.medINC.1C = gls(ln.Cseq ~ medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1C = lme(fixed = ln.Cseq ~ medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C, lme.medINC.1C)
#no difference in model fit

#use linear model
lm.medINC.1C = lm(ln.Cseq ~ medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1C = gls(ln.Cseq ~ medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1C = lme(fixed = ln.Cseq ~ medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C, lme.medAGE.1C)
#no difference in model fit

#use linear model
lm.medAGE.1C = lm(ln.Cseq ~ medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C)

##########################
#median resident duration#
##########################

#select random effects
gls.durC = gls(ln.Cseq ~ dur, data = sitr, na.action = na.omit)
lme.durC = lme(fixed = ln.Cseq ~ dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC, lme.durC)
#no difference in model fit

#use linear model
lm.durC = lm(ln.Cseq ~ dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC)

###########
#yard size#
###########

#select random effects
gls.ydC = gls(ln.Cseq ~ yd, data = sitr, na.action = na.omit)
lme.ydC = lme(fixed = ln.Cseq ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC, lme.ydC)
#no difference in model fit

#use linear model
lm.ydC = lm(ln.Cseq ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC)

#############
#############
#Yard Size  #
#############
#############

#############
#housing age#
#############

#select random effects
gls.Hageyd = gls(log(yd) ~Hage, data = sitr, na.action = na.omit)
lme.Hageyd = lme(fixed = log(yd) ~Hage, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.HageC, lme.HageC)
#no difference in model fit

#use linear model
lm.Hageyd = lm(log(yd) ~Hage, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.Hageyd)

################
#housing age ^2#
################

#select random effects
gls.Hageyd2 = gls(log(yd) ~Hage + I(Hage^2), data = sitr, na.action = na.omit)
lme.Hageyd2 = lme(fixed = log(yd) ~Hage + I(Hage^2), random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.Hageyd2, lme.Hageyd2)
#no difference in model fit

#use linear model
lm.Hageyd2 = lm(log(yd) ~Hage + I(Hage^2), data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.Hageyd2)

####################
#population density#
####################

#select random effects
gls.POP_Densityd = gls(log(yd) ~POP_Densit, data = sitr, na.action = na.omit)
lme.POP_Densityd = lme(fixed = log(yd) ~POP_Densit, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.POP_DensitC, lme.POP_DensitC)
#no difference in model fit

#use linear model
lm.POP_Densityd = lm(log(yd) ~POP_Densit, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.POP_DensitC)

##############
#percent silt#
##############

#select random effects
gls.Siltyd = gls(log(yd) ~Silt, data = sitr, na.action = na.omit)
lme.Siltyd = lme(fixed = log(yd) ~Silt, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.SiltC, lme.SiltC)
#no difference in model fit

#use linear model
lm.Siltyd = lm(log(yd) ~Silt, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.SiltC)

###############
#median income#
###############

#select random effects
gls.medINC.1yd = gls(log(yd) ~medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1yd = lme(fixed = log(yd) ~medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C, lme.medINC.1C)
#no difference in model fit

#use linear model
lm.medINC.1yd = lm(log(yd) ~medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C)

################
#assessed value#
################

#select random effects
gls.TotalValuayd = gls(log(yd) ~TotalValua, data = sitr, na.action = na.omit)
lme.TotalValuayd = lme(fixed = log(yd) ~TotalValua, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.TotalValuaC, lme.TotalValuaC)
#no difference in model fit

#use linear model
lm.TotalValuayd = lm(log(yd) ~TotalValua, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.TotalValuaC)

################
#percent vacant#
################

#select random effects
gls.perVACyd = gls(log(yd) ~perVAC, data = sitr, na.action = na.omit)
lme.perVACyd = lme(fixed = log(yd) ~perVAC, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perVACC, lme.perVACC)
#no difference in model fit

#use linear model
lm.perVACyd = lm(log(yd) ~perVAC, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perVACC)

##################
#percent minority#
##################

#select random effects
gls.perMINyd = gls(log(yd) ~perMIN, data = sitr, na.action = na.omit)
lme.perMINyd = lme(fixed = log(yd) ~perMIN, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMINC, lme.perMINC)
#no difference in model fit

#use linear model
lm.perMINyd = lm(log(yd) ~perMIN, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMINC)

#################
#percent married#
#################

#select random effects
gls.perMARyd = gls(log(yd) ~perMAR, data = sitr, na.action = na.omit)
lme.perMARyd = lme(fixed = log(yd) ~perMAR, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.perMARC, lme.perMARC)
#no difference in model fit

#use linear model
lm.perMARyd = lm(log(yd) ~perMAR, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.perMARC)

###############
#median income#
###############

#select random effects
gls.medINC.1yd = gls(log(yd) ~medINC.1, data = sitr, na.action = na.omit)
lme.medINC.1yd = lme(fixed = log(yd) ~medINC.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medINC.1C, lme.medINC.1C)
#no difference in model fit

#use linear model
lm.medINC.1yd = lm(log(yd) ~medINC.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medINC.1C)

#####################
#median resident age#
#####################

#select random effects
gls.medAGE.1yd = gls(log(yd) ~medAGE.1, data = sitr, na.action = na.omit)
lme.medAGE.1yd = lme(fixed = log(yd) ~medAGE.1, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.medAGE.1C, lme.medAGE.1C)
#no difference in model fit

#use linear model
lm.medAGE.1yd = lm(log(yd) ~medAGE.1, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.medAGE.1C)

##########################
#median resident duration#
##########################

#select random effects
gls.duryd = gls(log(yd) ~dur, data = sitr, na.action = na.omit)
lme.duryd = lme(fixed = log(yd) ~dur, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.durC, lme.durC)
#no difference in model fit

#use linear model
lm.duryd = lm(log(yd) ~dur, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.durC)

###########
#yard size#
###########

#select random effects
gls.ydyd = gls(yd ~ yd, data = sitr, na.action = na.omit)
lme.ydyd = lme(fixed = yd ~ yd, random = ~1|ID, data = sitr, na.action = na.omit)

#compare model fits using the likelihood ratio test
anova(gls.ydC, lme.ydC)
#no difference in model fit

#use linear model
lm.ydyd = lm(yd ~ yd, data = sitr, na.action = na.omit)

#verify homogeneity and normality of model residuals
plot(lm.ydC)
