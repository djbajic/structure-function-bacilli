source('functions.R')
d <- fread('data/comm_mixes_dMMfits.csv')

## .. to compare CFU in one and two-member communities 

d <- d[c.len<3 & replc==1]
d <- unique(d)

d <- melt(d, id.vars = c('comp', 'c.len'),
            measure.vars = c('C', 'E', 'M', 'P', 'S', 'T'))
d <- d[!is.na(value)]

aux.pair <- subset(d, c.len==2)
aux.sng <- subset(d, c.len==1)

aux.pair[, v1.alone := aux.sng$value[match(sapply(strsplit(comp, '-'), '[', 1),
                                           aux.sng$comp)]]
aux.pair[, v2.alone := aux.sng$value[match(sapply(strsplit(comp, '-'), '[', 2),
                                           aux.sng$comp)]]

aux.pair[, var := ifelse(variable == sapply(strsplit(comp, '-'), '[', 1), 'v1', 'v2')]

aux.pair <- dcast(aux.pair, ... ~ var, value.var='value')
aux.pair[, v1 := rep(v1[!is.na(v1)], each=2)]
aux.pair <- aux.pair[!is.na(v2)]
aux.pair <- melt(aux.pair[,c(1, 4:7)], id.vars = 'comp')
aux.pair[,value := as.numeric(value)]

aux.pair[, ':=' (v1 = sapply(strsplit(comp, '-'), '[', 1),
                 v2 = sapply(strsplit(comp, '-'), '[', 2))]

write.csv(aux.pair, 'data/two_sp_consortia.csv', row.names=FALSE)
