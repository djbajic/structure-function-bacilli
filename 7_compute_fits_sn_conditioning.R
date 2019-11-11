
# .. packages
source('functions.R')

d <- fread('data/data_SN_conditioning.csv')
d <- d[starch==TRUE]

fit.coefs <- d[, double.mm(x, f.j, 3),
               by = c('cond.SN', 'cond.SN.dil', 'strain', 'well')]
names(fit.coefs)[5:6] <- c('fit', 'fit.se')

write.csv(fit.coefs, 'data/SN_conditioning_dMMfits.csv', row.names=FALSE)
