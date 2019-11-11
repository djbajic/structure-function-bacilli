source('functions.R')
map <- fread('data/stepwise_map_PA.csv')

# .. pairwise epistasis heatmap 
map.1 <- copy(map)
map.1[, p.1 := sapply(strsplit(pair, '-'), '[',1)]
map.1[, p.2 := sapply(strsplit(pair, '-'), '[',2)]

map.2 <- copy(map)
map.2[, p.1 := sapply(strsplit(pair, '-'), '[',2)]
map.2[, p.2 := sapply(strsplit(pair, '-'), '[',1)]

aux <- rbind(map.1, map.2)

k <- subset(aux, is.na(bg))
k <- k[,.(sum(abs(epsilon))), by = p.1]
k <- k[order(V1, decreasing=TRUE)]
k <- k$p.1

aux[, p.1 := ordered(p.1, rev(k))]
aux[, p.2 := ordered(p.2, rev(k))]

clr <- brewer_pal('div', 5)(9)
p <- 'results/fig_2E.pdf'
pdf(p, 4.5, 4, family = 'CM Sans')
ggplot(subset(aux, is.na(bg) & p.1<p.2), aes(x=p.1, y=p.2, fill = epsilon)) +
    geom_tile(colour = NA) +
    scale_fill_gradient2(low = clr[1], mid = clr[5], high=clr[9], midpoint=0) +
    theme_bw() + labs(title = 'Pairwise epistasis', x = 'Species 1', y = 'Species 2') +
    theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)

# TO REMOVE
# pairwise.PA <- subset(aux, is.na(bg))
# save(pairwise.PA, file = 'pairwise_PA.Rdata')
