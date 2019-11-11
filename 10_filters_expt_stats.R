source('functions.R')
d <- fread('data/data_filters_test_3KDa_0.2uM.csv')

d <- d[, .(m0 = mean(t0),
           sd0 = sd(t0),
           m2 = mean(t2),
           sd2 = sd(t2),
           m18 = mean(t18),
           sd18 = sd(t18)),
         by=.(cond, sample)]

# normalize by initial starch concentration (i.e. convert to fraction starch degraded)
d[, m0.norm := m0/m0]
d[, m2.norm := m2/m0]
d[, m18.norm := m18/m0]

d[, sd0.norm := sqrt(2*((sd0/m0)^2))]
d[, sd2.norm := sqrt(((sd0/m0)^2) + ((sd2/m2)^2))]
d[, sd18.norm := sqrt(((sd0/m0)^2) + ((sd18/m18)^2))]

d.means = c('m0.norm', 'm2.norm', 'm18.norm')
d.errors = c('sd0.norm', 'sd2.norm', 'sd18.norm')

d = melt(d,
         measure.vars = list(d.means, d.errors),
         value.name = c('means', 'errors'))
write.csv(d, file = 'data/filters_processed.csv', row.names=FALSE) 
