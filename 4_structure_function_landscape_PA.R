source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')
map <- fread('data/stepwise_map_PA.csv')

# =============================================================================
# .. the "structure-function landscape" 
# =============================================================================

# .. instances with Vj = NA mean fit was impossible, so they have Vj = 0 
gr$se[which(is.na(gr$Vj.fit))] <- 0
gr$Vj.fit[which(is.na(gr$Vj.fit))] <- 0

# .. first build all possible trajectories (structure-function landscape)
strfun <- data.table(t(combn(gr$comp, 2)))
strfun[, len.v1 := gr[match(strfun$V1, gr$comp), c.len]]
strfun[, len.v2 := gr[match(strfun$V2, gr$comp), c.len]]
strfun.V1 <- with(strfun, ifelse(len.v1<len.v2, V1, V2))
strfun.V2 <- with(strfun, ifelse(len.v1<len.v2, V2, V1))
strfun$V1 <- strfun.V1
strfun$V2 <- strfun.V2
strfun <- strfun[,1:2]
strfun[, len.v1 := gr[match(strfun$V1, gr$comp), c.len]]
strfun[, len.v2 := gr[match(strfun$V2, gr$comp), c.len]]
strfun <- strfun[len.v2 == (len.v1+1)]

auxfun <- function(x, y) all(unlist(strsplit(x,'-')) %in% unlist(strsplit(y,'-')))
strfun[, all.in := auxfun(V1, V2), by = 1:nrow(strfun)]
strfun <- strfun[all.in==TRUE]

# .. get real Vj (for actual landscape) and standard errors
strfun$Vj.real.V1 <- gr$Vj.fit[match(strfun$V1, gr$comp)]
strfun$Vj.real.V2 <- gr$Vj.fit[match(strfun$V2, gr$comp)]

strfun$se.real.V1 <- gr$se[match(strfun$V1, gr$comp)]
strfun$se.real.V2 <- gr$se[match(strfun$V2, gr$comp)]

strfun <- unique(strfun)

# .. get str-fun landscape if everything was additive
aux <- gr[c.len==1 & replc == 1]

auxfun <- function(x) sum(aux$Vj.fit[match(unlist(strsplit(x, '-')), aux$comp)])
strfun[, Vj.1.V1 := auxfun(V1), by = 1:nrow(strfun)]
strfun[, Vj.1.V2 := auxfun(V2), by = 1:nrow(strfun)]

# errors
auxfun <- function(x) sqrt(sum(aux$se[match(unlist(strsplit(x, '-')), aux$comp)]^2))
strfun[, Vj.1.V1.se := auxfun(V1), by = 1:nrow(strfun)]
strfun[, Vj.1.V2.se := auxfun(V2), by = 1:nrow(strfun)]

# .. get str-fun landscape if truncating at pairwise
aux <- map[is.na(bg)]
pairwise <- function(x) {
    r <- unlist(strsplit(x, '-'))
    if(length(r)>1) {
        r <- apply(t(combn(r, 2)), 1, paste, collapse = '-')
        r.val <- sum(aux$epsilon[match(r, aux$pair)])
        r.err  <- sqrt(sum(aux$epsilon.se[match(r, aux$pair)]^2))
    } else {
        r.val <- 0
        r.err  <-  0
    }
    return(list(r.val, r.err))
}

strfun[, c('pair.int', 'pair.se'):= pairwise(V1), by = 1:nrow(strfun)]
strfun[, Vj.2.V1 := Vj.1.V1 + pair.int]
strfun[, Vj.2.V1.se := sqrt(Vj.1.V1.se^2 + pair.se^2)]
strfun[, c('pair.int', 'pair.se'):= NULL]

strfun[, c('pair.int', 'pair.se'):= pairwise(V2), by = 1:nrow(strfun)]
strfun[, Vj.2.V2 := Vj.1.V2 + pair.int]
strfun[, Vj.2.V2.se := sqrt(Vj.1.V2.se^2 + pair.se^2)]
strfun[, c('pair.int', 'pair.se'):= NULL]

# .. get Vj taking three-wise interactions into account
# .. first compute third-order interactions 
aux <- strfun[len.v2 == 3][,c('V2', 'Vj.2.V2', 'Vj.2.V2.se', 'Vj.real.V2', 'se.real.V2')]
aux <- unique(aux)
aux[, eps.3 := Vj.real.V2 - Vj.2.V2]
aux[, eps.3.se := sqrt(se.real.V2^2 + Vj.2.V2.se^2)]

auxfun <- function(x) {
    r <- unlist(strsplit(x, '-'))
    if(length(r)>2) {        
        r <- apply(t(combn(r, 3)), 1, paste, collapse = '-')
        r.val <- sum(aux$eps.3[match(r, aux$V2)], na.rm=TRUE)
        r.err  <- sqrt(sum(aux$eps.3.se[match(r, aux$V2)]^2, na.rm=TRUE))
    } else {
        r.val <- 0
        r.err  <-  0
    }
    return(list(r.val, r.err))
}


strfun[, c('pair.int', 'pair.se'):= auxfun(V1), by = 1:nrow(strfun)]
strfun[, Vj.3.V1 := Vj.2.V1 + pair.int]
strfun[, Vj.3.V1.se := sqrt(Vj.2.V1.se^2 + pair.se^2)]
strfun[, c('pair.int', 'pair.se'):= NULL]

strfun[, c('pair.int', 'pair.se'):= auxfun(V2), by = 1:nrow(strfun)]
strfun[, Vj.3.V2 := Vj.2.V2 + pair.int]
strfun[, Vj.3.V2.se := sqrt(Vj.2.V2.se^2 + pair.se^2)]
strfun[, c('pair.int', 'pair.se'):= NULL]


# .. add singles to the str-fun landscape 
aux <- strfun[len.v1==1]
aux <- aux[,c(2,1, 4,3,5, 7,6, 9,8, 11,10, 13,12, 16,17, 14,15, 20, 21, 18, 19)]
names(aux) <- names(strfun)
aux$V1 <- '-'
aux$len.v1 <- 0
aux$Vj.real.V1 <- 0
aux$se.real.V1 <- 0
aux$Vj.1.V1 <- 0
aux$Vj.1.V1.se <- 0
aux$Vj.2.V1 <- 0
aux$Vj.3.V1 <- 0

aux <- unique(aux)

strfun <- rbind(strfun, aux)

# .. variation upon species removal 
strfun[, vari := Vj.real.V2 - Vj.real.V1]
strfun[, poly := ifelse(V1 %like% 'P' & V2 %like% 'P', 'PP', 'N')]
strfun[, poly := ifelse(V2 %like% 'P' & poly=='N', 'P', poly)]
strfun[, l1 :=0]
strfun[, l2 :=1]

# .. interactions of higher order than 2
strfun[, eps.h := Vj.real.V2 - Vj.2.V2]
strfun[, eps.h.se := sqrt(se.real.V2^2 + Vj.2.V2.se^2)]
strfun[, eps.h.se := ifelse(len.v2 == 2, 0, eps.h.se)]

# .. interactions of higher order than 3
strfun[, eps.h3 := Vj.real.V2 - Vj.3.V2]
strfun[, eps.h3.se := sqrt(se.real.V2^2 + Vj.3.V2.se^2)]
strfun[, eps.h3.se := ifelse(len.v2 <= 3, 0, eps.h3.se)]

write.csv(strfun, 'data/structure_function_PA.csv', row.names=FALSE)
