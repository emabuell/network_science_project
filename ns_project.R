library(amen)
library(igraph)

A_usa <- read.csv('df_adj_usa_common.csv',header = T);
A_usa <- A_usa[,2:ncol(A_usa)]
rownames(A_usa) <- colnames(A_usa);
A_ch <- read.csv('df_adj_ch_common.csv',header = T);
A_ch <- A_ch[,2:ncol(A_ch)]
rownames(A_ch) <- colnames(A_ch);

#####################
# AME fitting USA
#####################

A_usa = A_usa/sum(A_usa);

for(j in 1:ncol(A_usa)){
  
  A_usa[j,j] = NA;
}

# fitting ame model on tasty
p.ame.usa <- ame(Y = as.matrix(A_usa,dimnames = list( colnames(A_usa),colnames(A_usa)  )),
                     family = 'bin',symmetric = T,dcor = FALSE,R = 2,burn = 100,gof = F);





#####################
# AME fitting China
#####################

A_ch = A_ch/sum(A_ch);

for(j in 1:ncol(A_ch)){
  
  A_ch[j,j] = NA;
}

# fitting ame model on tasty
p.ame.ch <- ame(Y = as.matrix(A_ch,dimnames = list( colnames(A_ch),colnames(A_ch)  )),
                 family = 'bin',symmetric = T,dcor = FALSE,R = 2,burn = 100,gof = F);




# plot of additive effects


dev.new()
par(mfrow = c(1,1))
plot(x = 0:(length(p.ame.usa$APM)-1),y = p.ame.usa$APM,xaxt = 'n' ,xlab = '',
     ylab = 'APM',
     main = 'posterior mean of a');
points(x = 0:(length(p.ame.ch$APM)-1),y = p.ame.ch$APM,xaxt = 'n' ,xlab = '',col = 'red');
axis(1, at=0:(length(p.ame.usa$APM)-1), labels=colnames(A_usa), cex.axis=0.6,srt = 45,
     las = 2);
abline(h = 0,col = 'Blue')
grid(nx = length(p.ame.usa$APM), ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("bottomright", legend=c("giallo", "tasty"),
       col=c("black", "red"),lty = 1, cex=0.6)


colnames(A_usa)[sign(p.ame.usa$APM) != sign(p.ame.ch$APM)]
