source('functions.R')

d <- fread('data/data_supernatants.csv')

## ==================================
## DOUBLE MICHAELIS MENTEN USING x*t
## ==================================

fit.coefs <- d[, double.mm(x, f.j, t), by = c('S1', 'S2')]

## single coefficients
s.coef <- fit.coefs[S1 == S2]
s.coef[, S2 := NULL]

# add OD from original culture
ods <- fread('data/data_SN_assay_pairwise_average_blank_subtracted.csv')
ods <- melt(ods, variable.factor=FALSE)
ods <- ods[, .(od.m = mean(value), od.sd = sd(value)), by=variable]
names(ods)[1] <- 'S1'

s.coef <- merge(s.coef, ods, by='S1')
write.csv(s.coef, file='data/singles_coefficients_fig_1.csv', row.names=FALSE)

aux <- s.coef
names(aux) <- c('S1', 'Vj.s1', 'se.s1', 'od.m', 'od.sd')
fit.coefs <- merge(fit.coefs, aux, by = c('S1'), all = TRUE)

names(aux) <- c('S2', 'Vj.s2', 'se.s2', 'od.m', 'od.sd')
fit.coefs <- merge(fit.coefs, aux, by = c('S2'), all = TRUE)

# .. Given that Vj = Kcat [E], and [E] will always be 1/2 in mixes,
fit.coefs[, Vj.pred := (Vj.s1 + Vj.s2)/2]
fit.coefs[, Vj.pred.se := sqrt(se.s1^2 + se.s2^2)] 

                                        # .. plot
use.xt <- rep(seq(0, 12, length.out=100), each=nrow(fit.coefs))
fit <- do.call("rbind", replicate(100, fit.coefs, simplify = FALSE))
fit[, xt := use.xt]

pred.f <- function(coef, xt)
{
    1 - ( 1 + coef * xt ) * exp( -coef * xt )
}

fit[, c('f.fitted', 'upper.f.fitted', 'lower.f.fitted') :=
          list(pred.f(Vj.fit, xt),
               pred.f(Vj.fit+(2*se), xt),
               pred.f(Vj.fit-(2*se), xt))]

fit[, c('f.pred', 'upper.f.pred', 'lower.f.pred') :=
          list(pred.f(Vj.pred, xt),
               pred.f(Vj.pred+(2*Vj.pred.se), xt),
               pred.f(Vj.pred-(2*Vj.pred.se), xt))]

write.csv(fit, file = 'data/supernatants_dMMfits.csv', row.names=FALSE)
