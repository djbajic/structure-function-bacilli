source('functions.R')
map <- fread('data/stepwise_map_PA.csv')
strfun <- fread('data/structure_function_PA.csv')

strains <- c( 'C', 'E', 'M', 'P', 'S', 'T')

# .. epistasis in pairs as a function of single background strains (3-body)
d <- map[bg %in% strains | is.na(bg)]
d[, bg := ifelse(is.na(bg), '-', bg)]
d[, bg := ordered(bg, c('-', sort(c( 'C', 'E', 'M', 'P', 'S', 'T'))))]

# .. strength of interactions in 3-body 
k <- cbind(do.call(rbind, strsplit(d$pair, '-')), as.character(d$bg))
k <- t(apply(k, 1, sort))
k <- apply(k, 1, paste, collapse = '-')
k <- strfun[match(k, V2)]
    
d <- cbind(d, k[, c('Vj.1.V2', 'Vj.1.V2.se',
                        'Vj.2.V2', 'Vj.2.V2.se',
                        'Vj.real.V2', 'se.real.V2')])
d[, eps.2 := (Vj.2.V2 - Vj.1.V2)]
d[, eps.2.se := sqrt(Vj.2.V2.se^2 + Vj.1.V2.se^2)]

write.csv(d, file='data/three_sp_consortia.csv', row.names=FALSE)
