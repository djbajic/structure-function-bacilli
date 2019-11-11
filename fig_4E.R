source('functions.R')

d <- fread('data/P_conditioning_dMMfits.csv')
d.summ <- d[, .(Vj.m = mean(Vj.fit),
                se = sd(Vj.fit)/2),
            by = c('cells', 'cond.SN')]
d.summ[, cond.SN := ordered(cond.SN, c('P', 'C', 'E', 'M', 'S', 'T'))]

p <- 'results/4E.pdf'
pdf(p, 4, 4, family = 'CM Sans')
ggplot(subset(d.summ, cells==TRUE), aes(x = cond.SN, y = Vj.m)) + 
geom_bar(stat ='identity') +
geom_errorbar(aes(ymax = Vj.m + se, ymin = Vj.m - se), width = .2) +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)


