source('functions.R')

gr <- fread('data/data_comm_mixes_growth.csv')
d <- fread('data/data_comm_mixes_starch.csv')

" there is some noise in the experimental results in starch measures
  for vol = 0 (i.e. no supernatant added, i.e. x==0). We decide that the best
  estimation is to average all x=0 wells and use that for all calculations
"
x.0 <- d[x == 0]
x.0 <- mean(x.0$abs)
d <- d[x != 0]

# .. compute the fraction of consumed starch (f.j) 
d <- d[, .(f.j = 1 - (abs - abs[starch==FALSE])/(x.0 - abs[starch==FALSE]),
           x = x, 
           starch = starch,
           comp = comp,
           c.len = c.len),
       by = c('well', 'plate', 'replc')]

d <- d[starch == TRUE]
d <- d[,starch := NULL]

# .. fit and add to gr dataframe
fit.coefs <- d[, double.mm(x, f.j, 2), by = c('well', 'plate')]
gr <- merge(gr, fit.coefs, by = c('well', 'plate'), all=TRUE)
gr <- gr[c.len > 0]

write.csv(gr, file='data/comm_mixes_dMMfits.csv', row.names=FALSE)
