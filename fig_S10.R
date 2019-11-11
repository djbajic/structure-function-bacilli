source('functions.R')
strfun <- fread('data/structure_function_PA.csv')

d <- unique(strfun, by = 'V2')
d[, .(rmsd = sqrt(sum(((Vj.1.V2 - Vj.real.V2)^2)/.N))), by = poly %like% 'P']
d[, table(poly %like% 'P')] # get the N 

d <- subset(strfun, poly=='N')
d <- d[,.(V2, Vj.1.V2, Vj.1.V2.se)]
d <- unique(d)

d.sat <- fread('data/communities_saturating_model_predictions.csv')
d.sat <- d.sat[, .(comp, satm.pred, satm.pred.se)]
d.sat <- d.sat[!(is.na(satm.pred))]
names(d.sat)[1] <- 'V2'
d <- merge(d, d.sat, by='V2', all=T)

p <- 'results/fig_S10.pdf'
pdf(p, 3, 3, family = 'CM Sans')
ggplot(subset(d, !(V2 %like% 'P')), aes(Vj.1.V2, satm.pred)) +
geom_point(colour = 'skyblue2', shape = 1) +
geom_errorbar(aes(ymin=satm.pred - satm.pred.se,
                  ymax=satm.pred + satm.pred.se),
              size=.3, width = 0, colour = 'skyblue2') +
geom_errorbarh(aes(xmin=Vj.1.V2 - Vj.1.V2.se,
                   xmax=Vj.1.V2 + Vj.1.V2.se), 
               size=.3, height = 0, colour = 'skyblue2') +
geom_abline(intercept = 0, slope=1, colour='red2', linetype='dashed') +
theme_bw() + labs(y = 'Saturating expression model' ,
                  x = 'Simple additive model ') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)

