#tfduca
library(ggm)

parcor <- function(Cor){
	Cor.inv <- solve(Cor)
	d <- sqrt(diag(Cor.inv))
	Parcor <- -Cor.inv / (d %*% t(d))
	diag(Parcor) <- 1
	Parcor
}

select.ij <- function(P, amat){
	p <- nrow(P); minabsP <- Inf
	for(i in (2:p)){
		for(j in (1:(i-1))){
			if(amat[i,j] == 1 && abs(P[i,j]) < minabsP){
				minabsP <- abs(P[i,j]); i0 <- i ; j0 <- j
			}
		}
	}
	c(i0, j0)
}


corselect <- function(X){
	R <- cor(X)
	n <- nrow(X)
	p <- ncol(R)
	G <- matrix(1,p,p) - diag(p)
	dimnames(G) <- dimnames(R)
	M <- R
	aic = 0
	result <- list()
	while(1){
		P <- parcor(M)
		select <- select.ij(P,G)
		G[select[1], select[2]] <- 0
		G[select[2], select[1]] <- 0
		f <- fitConGraph(G, R, n)
		M <- f$Shat
		aic_new = f$dev - 2*f$df
		if(aic_new > aic) break
		aic <- aic_new
		f <- c(f, list(remove = select, AIC = aic_new, Graph = G))
		result <- c(result, list(f))
	}
	result
}

X <- read.table("mark4.txt")
#X <- read.table("planted.txt")

print(summary(X))

pairs(X)
browser()

result <- corselect(X)

#[[t]]：t回目のループ実行後の状態．最適解まで表示．
print(result)
graph <- result[[length(result)]]$Graph

drawGraph(graph)
