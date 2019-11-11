source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')

# =============================================================================
# .. the "epistasis" map
# =============================================================================

# .. first get all backgrounds (i.e. combinations of <=5 strains, including 0)
strains <- c( 'P', 'C', 'E', 'M', 'S', 'T')

bg <- unique(gr$comp)
bg <- bg[sapply(strsplit(bg, '-'), length)<=5]
bg <- c(bg, NA)

# .. and the pairs
pair <- unique(gr$comp)
pair <- pair[sapply(strsplit(pair, '-'), length)==2]

# .. get map between pairs and all combinations of the rest of strains 
map <- data.table(expand.grid(bg, pair, stringsAsFactors = FALSE))
names(map) <- c('bg', 'pair')

auxfun <- function(x, y) any(unlist(strsplit(x,'-')) %in% unlist(strsplit(y,'-')))
map[, all.in := auxfun(pair, bg), by = 1:nrow(map)]
map <- map[all.in==FALSE]
map[, all.in :=NULL]

# .. background function
map[, bg.obs := gr[match(bg, comp)]$Vj.fit]
map[, bg.obs.se := gr[match(bg, comp)]$se]

map[, bg.obs := ifelse(is.na(bg), 0, bg.obs)]
map[, bg.obs.se := ifelse(is.na(bg), 0, bg.obs.se)]

# .. compute expected: observed V for + expected from singles, in background
r <- map[,.(paste(sort(c(unlist(strsplit(bg, '-')),
                         unlist(strsplit(pair, '-'))[1])), collapse = '-')),
         by = 1:nrow(map)]$V1

map[, single.1 := gr[match(r, comp)]$Vj.fit - bg.obs]
map[, s1.e := sqrt(gr[match(r, comp)]$se^2 + bg.obs.se^2)]

r <- map[,.(paste(sort(c(unlist(strsplit(bg, '-')),
                         unlist(strsplit(pair, '-'))[2])), collapse = '-')),
         by = 1:nrow(map)]$V1

map[, single.2 := gr[match(r, comp)]$Vj.fit - bg.obs]
map[, s2.e := sqrt(gr[match(r, comp)]$se^2 + bg.obs.se^2)]

map[, expected.V := single.1 + single.2]
map[, expected.se := sqrt(s1.e^2 + s2.e^2)]

# .. add the observed one to the table 
r <- map[,.(paste(sort(c(unlist(strsplit(bg, '-')),
                         unlist(strsplit(pair, '-')))), collapse = '-')),
         by = 1:nrow(map)]$V1

map[, observed.V := gr[match(r, comp)]$Vj.fit - bg.obs]
map[, observed.se := sqrt(gr[match(r, comp)]$se^2 + bg.obs.se^2)]

# .. compute epsilon
map[, epsilon := observed.V - expected.V]
map[, epsilon.se := sqrt(observed.se^2 + expected.se^2)]

# .. add column for epsilon^0
aux <- map[is.na(bg)]
aux <- aux[, c('pair', 'epsilon', 'epsilon.se')]
names(aux)[2:3] <- c('eps.zero', 'eps.zero.se')
map <- merge(map, aux, by = 'pair', all=TRUE)

# .. add background community size
map[, bg.size := sapply(strsplit(bg, '-'), length)]
map[, bg.size := ifelse(is.na(bg), 0, bg.size)]

# .. delta_epsilon_ABC
map[, d.epsilon.ABC := epsilon-eps.zero]
map[, d.epsilon.ABC.se := sqrt(epsilon.se^2 + eps.zero.se^2)]

write.csv(map, 'data/stepwise_map_PA.csv', row.names=FALSE)
