structure_plot <- function(names, ninds = 60, klist){
	# define colors
	cols <- c('#A8FFFD', '#B862D3','#A39D9D','#FFFF00', '#ff5a5a', '#69C261', '#26CDCD', '#C1C6FF')
	# unique label names
	sp.names <- as.character(unique(names))
	#n <- as.data.frame(matrix(ncol = 1, nrow = ninds))
	#n[,1] <- names
	# locations of each column
	b <- as.data.frame(matrix(ncol = 1, nrow = ninds))
	b[,1] <- barplot(t(klist[[1]][1]), beside= F, col= cols, cex.name= 1, cex.axis= 1.2, border = 1, space = 0.05, xaxt = 'n', yaxt = 'n', cex.lab = 1, cex.main = 2)
	# find locations for labels in the barplot
	my.mean <- tapply(X = b[,1], INDEX = n, mean)
	my.min <- tapply(X = b[,1], INDEX = n, min)
	my.max <- tapply(X = b[,1], INDEX = n, max)
	# data frame for plotting
	d <- sp_labels(names = sp.names, min = my.min, mean = my.mean, max = my.max)
	# plot
	plot_q_per_chain(klist)
}

# create labels
sp_labels <- function(names, min, mean, max, ...){
	d <- as.data.frame(matrix(nrow = length(names), ncol = 4))
	for (j in 1:length(names)){
			d[j,1] <- names[j]
			d[j,3] <- min[[j]][1]
			d[j,2] <- mean[[j]][1]
			d[j,4] <- max[[j]][1]
	}
	return(d)
}

# plot chains with species and geography labels 
plot_q_per_chain <- function(kqlist, ...){
	cols <- c('#a8fffd', '#b862d3','#a39d9d','#ffff00', '#ff5a5a', '#69c261', '#26cdcd', '#c1c6ff') 	
	par(mfrow = c(length(kqlist),1), mar = c(1,3,3,1) + 0.1, oma = c(15,0,0,0), mgp = c(1,1,0))
	chain <- seq(1, length(kqlist), 1) 
	for(i in 1:length(kqlist)){
		barplot(t(kqlist[[i]]), beside= F, col= cols, border = 1, space = 0.05, xaxt = 'n', yaxt = 'n', main = paste("k =", chain[i]+1, sep = ' '), cex.lab = 1.2, cex.main = 1.6)
		# y axis
		axis(2, at = c(0, 0.25, 0.5, 0.75, 1), cex.axis = 1, las = 2, pos = -0.2)
 	}
	# x axis, rotating labels
	# species labels
	text(cex = 1.5, x = (d[,2]-0.3), y = -0.7, labels = d[,1], xpd=NA, srt=50, font=3)
	# lines
	for (i in 1:length(d[,1])){
		lines(x = d[i,3:4] , y = rep(-0.1, 2), lwd = 2.5, col = "black", xpd = NA)
	}
	# location labels (make new df for this one?)
	#text(cex = 1.7, x = (e[,3]-4), y = -0.9, labels = e[,1], xpd=na, srt=50, font=3)

}
