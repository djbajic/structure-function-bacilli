source('functions.R')
library(minpack.lm)
library(propagate)
fit.coefs <- fread('data/SN_conditioning_dMMfits.csv')

d <- fread('data_SN_conditioning.csv')
d <- d[starch==TRUE]
d <- d[x>0]
d <- d[, c('cond.SN', 'cond.SN.dil', 'strain', 'od600.m', 'od600.sd', 'well', 'cfu.m', 'cfu.sd')]
d <- unique(d)
d <- d[cond.SN!='B']

d <- merge(fit.coefs, d,
             by = c('cond.SN', 'cond.SN.dil', 'strain', 'well'), all=TRUE)

d[, 'REP' := ifelse(well %in% grep('01|02|03|04|05|06', d$well, value = TRUE),
                      1, 2)]
d <- d[!is.na(cond.SN)]

d[, fit := ifelse(is.na(fit), 0, fit)]
d[, cfu.se := cfu.sd/sqrt(10)]


#==============================================================================
#                  FITTING A LANGMUIR MODEL TO DATA 
#==============================================================================

## obtain now the fit of the langmuir-type model for each strain

# polymyxa fitting
k <- d[strain == 'P']
k <- k[cond.SN != 'E']
write.csv(k, file = 'data/P_langm_data.csv', row.names=FALSE)

k.fit.P <- with(k, langmuir.model(fit, cfu.m))
fit.P <- summary(k.fit.P)$parameters
fit.P <- c(fit.P[2,1:2], fit.P[3,1:2])

fitline  <- data.table(N = seq(100, max(k$cfu.m), length=200))
fitline.ci <- predictNLS(k.fit.P, newdata = fitline)

fitline <- cbind(fitline, fitline.ci$summary)
names(fitline)[6:7] <- c('prop2.5', 'prop97.5')
names(fitline)[12:13] <- c('sim2.5', 'sim97.5')

fitline[, prop2.5 := ifelse(prop2.5<.1, .1, prop2.5)]
write.csv(fitline, file = 'data/P_langm_fitline.csv', row.names=FALSE)

# cereus fitting
k <- d[strain == 'C' & fit>0]
write.csv(k, file = 'data/C_langm_data.csv', row.names=FALSE)

fit.C <- c(mean(k$fit), sd(k$fit), min(k$cfu.m)/2, 0)
fitline  <- data.table(cfu.m = seq(0, max(k$cfu.m), 10))
fitline[, predicted := fit.C[1] * cfu.m/(cfu.m + fit.C[3])]
write.csv(fitline, file = 'data/C_langm_fitline.csv', row.names=FALSE)

# megaterium fitting
k <- d[strain == 'E' & REP==1] # rep 2 is a failed replicate 
write.csv(k, file = 'data/E_langm_data.csv', row.names=FALSE)

fit.E <- c(mean(k$fit), sd(k$fit), min(k$cfu.m)/2, 0)
fitline  <- data.table(cfu.m = seq(0, max(k$cfu.m), 10))
fitline[, predicted := fit.E[1] * cfu.m/(cfu.m + fit.E[3])]
write.csv(fitline, file = 'data/E_langm_fitline.csv', row.names=FALSE)

# mojavensis fitting
k <- d[strain == 'M' & fit>0]
write.csv(k, file = 'data/M_langm_data.csv', row.names=FALSE)

fit.M <- c(mean(k$fit), sd(k$fit), min(k$cfu.m)/2, 0)
fitline  <- data.table(cfu.m = seq(0, max(k$cfu.m), 10))
fitline[, predicted := fit.M[1] * cfu.m/(cfu.m + fit.M[3])]
write.csv(fitline, file = 'data/M_langm_fitline.csv', row.names=FALSE)

# subtilis fitting
k <- d[strain == 'S']
write.csv(k, file = 'data/S_langm_data.csv', row.names=FALSE)

fit.S <- c(mean(k$fit), sd(k$fit), min(k$cfu.m)/2, 0)
fitline  <- data.table(cfu.m = seq(0, max(k$cfu.m), 10))
fitline[, predicted := fit.S[1] * cfu.m/(cfu.m + fit.S[3])]
write.csv(fitline, file = 'data/S_langm_fitline.csv', row.names=FALSE)

# T fitting
k <- d[strain == 'T']
write.csv(k, file = 'data/T_langm_data.csv', row.names=FALSE)

fit.T <- c(mean(k$fit), sd(k$fit), min(k$cfu.m)/2, 0)
fitline  <- data.table(cfu.m = seq(0, max(k$cfu.m), 10))
fitline[, predicted := fit.T[1] * cfu.m/(cfu.m + fit.T[3])]
write.csv(fitline, file = 'data/T_langm_fitline.csv', row.names=FALSE)


# save data from fits 
langm.pred <- data.table(strain = c( 'C', 'E', 'M', 'P', 'S', 'T'),
                         rbind(fit.C, fit.E, fit.M, fit.P, fit.S, fit.T))
names(langm.pred) <- c('strain', 'epsilon', 'epsilon.sd',
                       'K', 'K.sd')
write.csv(langm.pred, 'data/langmuir_null_predictions.csv', row.names=FALSE) 
