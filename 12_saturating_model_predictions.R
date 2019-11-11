# .. analyze supernatants starch consumption 
source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')

species <- c('C', 'E', 'M', 'P', 'S', 'T')

# compute values observed using the saturating model with population sizes x
langm.pred <- fread('data/langmuir_null_predictions.csv')

# .. singles 
singles <- gr[c.len==1 & replc==1, ]

cols <- species
cell.counts <- melt(singles[, ..cols], measure.vars=cols)
cell.counts <- cell.counts[!is.na(value)]
names(cell.counts) <- c('strain', 'N')

cols.sd <- paste0('sd.', cols)
cell.sd <- melt(singles[, ..cols.sd], measure.vars=cols.sd)
cell.sd <- cell.sd[!is.na(value)]
names(cell.sd) <- c('strain', 'N.sd')
cell.sd$strain <- cols

langm.pred <- merge(langm.pred, cell.counts)
langm.pred <- merge(langm.pred, cell.sd)

langm.pred[, pred := epsilon*N/(N + K)]
langm.pred[, pred.se := sqrt(((N/(N+K))^2)*(epsilon.sd^2) +
                            (epsilon*(K/((N+K)^2)))^2*(N.sd^2) +
                            (epsilon*(N/((N+K)^2)))^2*(K.sd^2))]

newcols <- c(paste(species, 'pred', 'comm', sep='.'),
             paste(species, 'pred', 'comm.se', sep='.'),
             paste(species, 'pred', 'sng', sep='.'),
             paste(species, 'pred', 'sng.se', sep='.'),
             'satm.pred', 'satm.pred.se')

gr[, (newcols) := numeric()]

for (i in 1:nrow(gr)) {

    cols <- strsplit(gr[i, comp], '-')[[1]]
    pred.data  <-  langm.pred[match(cols, strain)]

    # update gr with pred.singl values 
    curr.cols <- paste(pred.data$strain, 'pred', 'sng', sep='.') 
    gr[i,  (curr.cols) := as.list(pred.data$pred)]

    curr.cols <- paste(pred.data$strain, 'pred', 'sng.se', sep='.') 
    gr[i,  (curr.cols) := as.list(pred.data$pred.se)]

    # now use the N values from the consortia to compute pred.comm
    cell.counts <- as.numeric(gr[i, ..cols])

    cols.sd <- paste0('sd.', cols)
    cell.sd <- as.numeric(gr[i, ..cols.sd])

    pred.data[, N := cell.counts]
    pred.data[, N.sd := cell.sd]

    pred.data[, pred := epsilon*N/(N + K)]
    pred.data[, pred.se := sqrt(((N/(N+K))^2)*(epsilon.sd^2) +
                                (epsilon*(K/((N+K)^2)))^2*(N.sd^2) +
                                (epsilon*(N/((N+K)^2)))^2*(K.sd^2))]

    gr$satm.pred[i] <- with(pred.data, sum(pred))
    gr$satm.pred.se[i] <- with(pred.data, sum(pred.se))

    curr.cols <- paste(pred.data$strain, 'pred', 'comm', sep='.') 
    gr[i,  (curr.cols) := as.list(pred.data$pred)]

    curr.cols <- paste(pred.data$strain, 'pred', 'comm.se', sep='.') 
    gr[i,  (curr.cols) := as.list(pred.data$pred.se)]
}


# compute now the deltas and their errors
for (i in species) {

    gr[, (paste0(i, '.delta')) :=
         get(paste0(i, '.pred.comm')) - get(paste0(i, '.pred.sng'))]

    gr[, (paste0(i, '.delta.se')) :=
         sqrt(get(paste0(i, '.pred.comm.se'))^2 +
              get(paste0(i, '.pred.sng.se'))^2)]
}


# compute bounds for P behavioral interaction in communities 
gr[, sum.deltas := sum(C.delta, E.delta, M.delta,
                        P.delta, S.delta, T.delta,
                        na.rm=TRUE), by=1:NROW(gr)]

gr[, sum.deltas.se := sqrt(sum(C.delta.se^2, E.delta.se^2, M.delta.se^2,
                                P.delta.se^2, S.delta.se^2, T.delta.se^2,
                                na.rm=TRUE)), by=1:NROW(gr)]

# .. get expected under P/A null model (previously called Vj.1.V1 and Vj.1.V2 in strfun)
aux <- gr[c.len==1 & replc == 1]

auxfun <- function(x) sum(aux$Vj.fit[match(unlist(strsplit(x, '-')), aux$comp)])
gr[, PA.pred := auxfun(comp), by = 1:nrow(gr)]

# errors
auxfun <- function(x) sqrt(sum(aux$se[match(unlist(strsplit(x, '-')), aux$comp)]^2))
gr[, PA.pred.se := auxfun(comp), by = 1:nrow(gr)]


# Compute the pairwise interactions, epsilon.pop and epsilon.behav
gr.pairs <- subset(gr, c.len==2 & replc==1)

gr.pairs[, PA.epsilon.tot := Vj.fit - PA.pred]
gr.pairs[, PA.epsilon.tot.se := sqrt(se^2 + PA.pred.se^2)]

gr.pairs[, epsilon.pop := sum.deltas] # eq. 27 
gr.pairs[, epsilon.pop.se := sum.deltas.se]

gr.pairs[, epsilon.behav := Vj.fit - PA.pred - epsilon.pop]
gr.pairs[, epsilon.behav.se := sqrt(se^2 + PA.pred.se^2 + epsilon.pop.se^2)]

gr.pairs[, epsilon.pop.eq29 := PA.epsilon.tot - epsilon.behav] # eq. 29 
gr.pairs[, epsilon.pop.eq29.se := sqrt(PA.epsilon.tot.se^2 + epsilon.behav.se^2)] # eq. 29 


# compute pairwise interactions in all communities (sum.epsilon.ij)
pairwise <- function(x) {
    r <- unlist(strsplit(x, '-'))
    if(length(r)>1) {
        r <- apply(t(combn(r, 2)), 1, paste, collapse = '-')

        sum.PA.pairwise <- gr.pairs[comp %in% r, sum(PA.epsilon.tot)]
        sum.PA.pairwise.se  <- sqrt(gr.pairs[comp %in% r, sum(PA.epsilon.tot.se^2)])

        sum.pairwise.pop <- gr.pairs[comp %in% r, sum(epsilon.pop)]
        sum.pairwise.pop.se <- sqrt(gr.pairs[comp %in% r, sum(epsilon.pop.se^2)])        
       
    } else {
        sum.PA.pairwise <- 0
        sum.PA.pairwise.se  <- 0 

        sum.pairwise.pop <- 0
        sum.pairwise.pop.se <- 0
    }
    return(list(sum.PA.pairwise, sum.PA.pairwise.se,
                sum.pairwise.pop, sum.pairwise.pop.se))
}

gr[, c('sum.PA.pairwise', 'sum.PA.pairwise.se',
       'sum.pairwise.pop', 'sum.pairwise.pop.se') := pairwise(comp),
   by = 1:nrow(gr)]

# compute higher order interactions
gr[, PA.H.pairwise.tot := Vj.fit - PA.pred - sum.PA.pairwise]
gr[, PA.H.pairwise.tot.se := sqrt(se^2 + PA.pred.se^2 + sum.PA.pairwise.se^2)]

gr[, H.pairwise.pop := sum.deltas - sum.pairwise.pop] 
gr[, H.pairwise.pop.se := sqrt(sum.deltas.se^2 + sum.pairwise.pop.se^2)] 

gr[, H.pairwise.behav := PA.H.pairwise.tot - H.pairwise.pop] 
gr[, H.pairwise.behav.se := sqrt(PA.H.pairwise.tot.se^2 + H.pairwise.pop.se^2)] 


write.csv(gr.pairs, file = 'data/pairs_saturating_model_predictions.csv',
          row.names=FALSE)
write.csv(gr, file = 'data/communities_saturating_model_predictions.csv',
          row.names=FALSE)




## source('functions.R')
## gr <- fread('data/comm_mixes_dMMfits.csv')
## langm.pred <- fread('data/langmuir_null_predictions.csv')


## # now try to predict the langmuir/stepwise fitted langmuir from supernatants 

## gr <- gr[,n.pred := NA][replc == 1]
## for (i in 1:nrow(gr)) {

##     cols <- strsplit(gr[i, comp], '-')[[1]]
##     cell.counts <- as.numeric(gr[i, ..cols])

##     cols.sd <- paste0('sd.', cols)
##     cell.sd <- as.numeric(gr[i, ..cols.sd])

##     pred.data  <-  langm.pred[match(cols, strain)]
##     pred.data[, N := cell.counts]
##     pred.data[, N.sd := cell.sd]

##     pred.data[, pred := epsilon*N/(N + K)]
##     pred.data[, pred.se := sqrt(((N/(N+K))^2)*(epsilon.sd^2) +
##                                 (epsilon*(K/((N+K)^2)))^2*(N.sd^2) +
##                                 (epsilon*(N/((N+K)^2)))^2*(K.sd^2))]

##     gr$n.pred[i] <- with(pred.data, sum(pred))
##     gr$n.pred.se[i] <- with(pred.data, sqrt(sum(pred.se^2)))
## }
