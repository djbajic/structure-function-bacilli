
source('functions.R')
d <- fread('data/data_P_conditioning_activity.csv')
d <- d[, .(f.j = 1 - (abs - abs[starch==FALSE])/(abs[cond.SN.dil==0] - abs[starch==FALSE]),
                 x = cond.SN.dil/200, 
                 starch = starch),
             by = c('cond.SN', 'replc', 'cells')]

d <- d[starch == TRUE]
d <- d[,starch := NULL]

fits <- d[, double.mm(x, f.j, 2), by = c('cond.SN', 'replc', 'cells')]
write.csv(fits, 'data/P_conditioning_dMMfits.csv', row.names=FALSE)
