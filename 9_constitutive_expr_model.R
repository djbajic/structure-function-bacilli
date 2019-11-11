source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')

# fitted single strain coefficients from fig 1
# we use these because we consider them more reliable tahn those from 
# the consortia experiment, as in fig. 1 x*t goes to 12 
s.coef <- fread('data/singles_coefficients_fig_1.csv')
names(s.coef)[1] <- 'Strain'

# od - cfu correspondence experiment 
od.cfu <- fread('data/od_cfu.csv')
od.cfu[, 'cfu.od' := cfu.ml/OD]

s.coef <- merge(s.coef, od.cfu[, c('Strain', 'cfu.od')], by='Strain')
s.coef[, cfu.m := cfu.od*od.m]
s.coef[, cfu.sd := cfu.od*od.sd]

# calculate epsion and its error
s.coef[, epsilon := Vj.fit/cfu.m]
s.coef[, epsilon.se := epsilon*sqrt((se/Vj.fit)^2 + (cfu.sd/cfu.m)^2)]


# try to predict function in combinations using N * epsilon model
k <- gr[c.len==1 & replc == 1]
k[, CFU := .SD[[comp]], by=comp][]
k[, CFU := as.numeric(CFU)]

k[, epsilon := Vj.fit/CFU]
k[, epsilon.se := se/CFU]

aux <- gr[,n.pred := NA][replc == 1] 
for (i in 1:nrow(aux)) {
    cols <- strsplit(aux[i, comp], '-')[[1]]
    cell.counts <- as.numeric(aux[i, ..cols])

    cols.sd <- paste0('sd.', cols)
    cell.sd <- as.numeric(aux[i, ..cols.sd])

    epsilon  <-  k[match(cols, comp), epsilon]
    epsilon.se  <-  k[match(cols, comp), epsilon.se]

    aux$n.pred[i] <- sum(epsilon * cell.counts)

    # first, keep only those species with meaningful values 
    nonzeros <- which(epsilon>0 & epsilon.se>0 & cell.counts>0)
    epsilon <- epsilon[nonzeros]
    epsilon.se <- epsilon.se[nonzeros]
    cell.counts <- cell.counts[nonzeros]
    cell.sd <- cell.sd[nonzeros]

    # error for each species, add errors in epsilon and errors in cell count
    sp.errors <- (epsilon*cell.counts) * sqrt((epsilon.se/epsilon)^2 +
                                              (cell.sd/cell.counts)^2)
    aux$n.pred.se[i] <- sqrt(sum(sp.errors^2))
}

write.csv(aux, 'data/constitutive_expr_model.csv', row.names=FALSE)
