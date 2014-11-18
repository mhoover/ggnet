### determines coordinates of plot
coord.place <- function(network, layout = "fruchtermanreingold", 
	plot.layout = NULL) {

# load dependencies
	require(ggplot2, quietly = TRUE)
	require(grid, quietly = TRUE)
	require(statnet, quietly = TRUE)

# check for no ties in graph
	if(sum(as.matrix(network)) == 0) {
		add.edges(network, 1, 2)
	}
	
# determine graph layout algorithm
	graph.layout <- match.fun(paste("gplot.layout.", layout, sep = ""))

# create data frame of node coordinates (based on graph layout algorithm)
	nodes <- data.frame(graph.layout(network, plot.layout))
	names(nodes) <- c("x", "y")
	return(nodes)
}

### plots network allowing for a variety of attribute options
ggnet <- function(net, direct = FALSE, color = NULL, names = NULL, 
	shape = NULL, size = NULL, edge.val = NULL, title = NULL, legend = FALSE, 
	coords = NULL, layout = "fruchtermanreingold", plot.layout = NULL, 
	palette = "Set1", gradient = FALSE, alpha = 1) {

# load dependencies
	require(ggplot2, quietly = TRUE)
	require(grid, quietly = TRUE)
	require(statnet, quietly = TRUE)

# determine plot coordinates
	if(is.null(coords)) {
		if(sum(as.matrix(net)) == 0) {
			add.edges(net, 1, 2)
			nodes <- coord.place(network = net, layout = layout, plot.layout = 
				plot.layout)
			delete.edges(net, 1)
		} else {
			nodes <- coord.place(network = net, layout = layout, plot.layout = 
				plot.layout)
		}
	} else {
		nodes <- coords
	}

# plot empty graph
	if(sum(as.matrix(net)) == 0) {
# determine node color
		if(gradient == TRUE) {
			nodes$color <- as.numeric(net %v% color)
		} else {
			if(is.null(color)) {
				nodes$color <- as.factor(1)
				col.show <- "none"
			} else {
				nodes$color <- as.factor(net %v% color)
				col.show <- "legend"
			}
		}

# determine node names
		if(is.null(names)) {
			nodes$name <- ""
		} else {
			nodes$name <- net %v% names
		}
	
# determine node shape
		if(is.null(shape)) {
			nodes$shape <- as.factor(1)
			shp.show <- "none"
		} else {
			nodes$shape <- as.factor(net %v% shape)
			shp.show <- "legend"
		}

# determine legend 
		legend.place <- ifelse(legend == TRUE, "right", "none")

# create network plot
		if(gradient == TRUE) {
			if(is.null(size)) {
				graphic <- ggplot() + 
					geom_point(aes(x, y, color = color, shape = shape), 
						alpha = alpha, size = 3, data = nodes) +
					geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
						size = 3, color = "grey25") + 

					scale_color_gradient(low = "blue", high = "red", 
						name = color) +
					scale_shape(name = shape, guide = shp.show) + 
					scale_x_continuous(breaks = NULL) + 
					scale_y_continuous(breaks = NULL) + 
					labs(title = title) + 

					theme(panel.background = element_rect(fill = "white", 
						color = NA), 
					axis.title.x = element_blank(), 
					axis.title.y = element_blank(), 
					legend.position = legend.place, 
					legend.background = element_rect(color = NA))
				return(print(graphic))
			} else {
				nodes$size <- net %v% size

				graphic <- ggplot() + 
					geom_point(aes(x, y, color = color, shape = shape, 
						size = size), alpha = alpha, data = nodes) +
					geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
						size = 3, color = "grey25") + 

					scale_color_gradient(low = "blue", high = "red", 
						name = color) + 
					scale_shape(name = shape, guide = shp.show) + 
					scale_size(name = size) + 
					scale_x_continuous(breaks = NULL) + 
					scale_y_continuous(breaks = NULL) + 
					labs(title = title) + 

					theme(panel.background = element_rect(fill = "white", 
						color = NA), 
					axis.title.x = element_blank(), 
					axis.title.y = element_blank(), 
					legend.position = legend.place, 
					legend.background = element_rect(color = NA))
				return(print(graphic))
			}
		} else {
			if(is.null(size)) {
				graphic <- ggplot() + 
					geom_point(aes(x, y, color = color, shape = shape), 
						alpha = alpha, size = 3, data = nodes) +
					geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
						size = 3, color = "grey25") + 

					scale_color_brewer(palette = palette, name = color, 
						guide = col.show) + 
					scale_shape(name = shape, guide = shp.show) + 
					scale_x_continuous(breaks = NULL) + 
					scale_y_continuous(breaks = NULL) + 
					labs(title = title) + 

					theme(panel.background = element_rect(fill = "white", 
						color = NA), 
					axis.title.x = element_blank(), 
					axis.title.y = element_blank(), 
					legend.position = legend.place, 
					legend.background = element_rect(color = NA))
				return(print(graphic))
			} else {
				nodes$size <- net %v% size

				graphic <- ggplot() + 
					geom_point(aes(x, y, color = color, shape = shape, 
						size = size), alpha = alpha, data = nodes) +
					geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
						size = 3, color = "grey25") + 

					scale_color_brewer(palette = palette, name = color, 
						guide = col.show) + 
					scale_shape(name = shape, guide = shp.show) + 
					scale_size(name = size) + 
					scale_x_continuous(breaks = NULL) + 
					scale_y_continuous(breaks = NULL) + 
					labs(title = title) + 

					theme(panel.background = element_rect(fill = "white", 
						color = NA), 
					axis.title.x = element_blank(), 
					axis.title.y = element_blank(), 
					legend.position = legend.place, 
					legend.background = element_rect(color = NA))
				return(print(graphic))
			}
		}
	} else {
# create data frame of edge coordinates from node to node
		if(is.null(edge.val)) {
			el <- as.edgelist(net)
			edges <- data.frame(nodes[el[, 1], ], nodes[el[, 2], ])
			edges$val <- 2
			names(edges)[1:4] <- c("x.beg", "y.beg", "x.end", "y.end")
		} else {
			el <- as.edgelist(net)
			edges <- data.frame(nodes[el[, 1], ], nodes[el[, 2], ])
			if(direct == FALSE) {
				edge.val[upper.tri(edge.val)] <- 0
			}
			edges$val <- c(edge.val[edge.val > 0])
			names(edges)[1:4] <- c("x.beg", "y.beg", "x.end", "y.end")
		}

# determine arrows for directed network
		arr.head <- ifelse(direct == TRUE, .2, 0)

# determine node color
		if(gradient == TRUE) {
			nodes$color <- as.numeric(net %v% color)
		} else {
			if(is.null(color)) {
				nodes$color <- as.factor(1)
				col.show <- "none"
			} else {
				nodes$color <- as.factor(net %v% color)
				col.show <- "legend"
			}
		}

# determine node names
		if(is.null(names)) {
			nodes$name <- ""
		} else {
			nodes$name <- net %v% names
		}
	
# determine node shape
		if(is.null(shape)) {
			nodes$shape <- as.factor(1)
			shp.show <- "none"
		} else {
			nodes$shape <- as.factor(net %v% shape)
			shp.show <- "legend"
		}

# determine legend 
		legend.place <- ifelse(legend == TRUE, "right", "none")
		
# create network plot
		if(gradient == TRUE) {
			if(is.null(size)) {
				graphic <- ggplot() + 
					geom_segment(aes(x = x.beg, y = y.beg, xend = x.end, 
						yend = y.end), data = edges, size = 0.35, 
						color = "grey", alpha = edges$val / max(edges$val), 
						arrow = arrow(length = unit(arr.head, "cm"))) + 
					geom_point(aes(x, y, color = color, shape = shape), 
						alpha = alpha, size = 3, data = nodes) +
					geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
						size = 3, color = "grey25") + 

					scale_color_gradient(low = "blue", high = "red", 
						name = color) +
					scale_shape(name = shape, guide = shp.show) + 
					scale_x_continuous(breaks = NULL) + 
					scale_y_continuous(breaks = NULL) + 
					labs(title = title) + 

					theme(panel.background = element_rect(fill = "white", 
						color = NA), 
					axis.title.x = element_blank(), 
					axis.title.y = element_blank(), 
					legend.position = legend.place, 
					legend.background = element_rect(color = NA))
				return(print(graphic))
			} else {
				nodes$size <- net %v% size

				graphic <- ggplot() + 
					geom_segment(aes(x = x.beg, y = y.beg, xend = x.end, 
						yend = y.end), data = edges, size = 0.35, 
						color = "grey", alpha = edges$val / max(edges$val), 
						arrow = arrow(length = unit(arr.head, "cm"))) + 
					geom_point(aes(x, y, color = color, shape = shape, 
						size = size), alpha = alpha, data = nodes) +
					geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
						size = 3, color = "grey25") + 

					scale_color_gradient(low = "blue", high = "red", 
						name = color) + 
					scale_shape(name = shape, guide = shp.show) + 
					scale_size(name = size) + 
					scale_x_continuous(breaks = NULL) + 
					scale_y_continuous(breaks = NULL) + 
					labs(title = title) + 

					theme(panel.background = element_rect(fill = "white", 
						color = NA), 
					axis.title.x = element_blank(), 
					axis.title.y = element_blank(), 
					legend.position = legend.place, 
					legend.background = element_rect(color = NA))
				return(print(graphic))
			}
		} else {
			if(is.null(size)) {
				graphic <- ggplot() + 
					geom_segment(aes(x = x.beg, y = y.beg, xend = x.end, 
						yend = y.end), data = edges, size = 0.35, 
						color = "grey", alpha = edges$val / max(edges$val), 
						arrow = arrow(length = unit(arr.head, "cm"))) + 
					geom_point(aes(x, y, color = color, shape = shape), 
						alpha = alpha, size = 3, data = nodes) +
					geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
						size = 3, color = "grey25") + 

					scale_color_brewer(palette = palette, name = color, 
						guide = col.show) + 
					scale_shape(name = shape, guide = shp.show) + 
					scale_x_continuous(breaks = NULL) + 
					scale_y_continuous(breaks = NULL) + 
					labs(title = title) + 

					theme(panel.background = element_rect(fill = "white", 
						color = NA), 
					axis.title.x = element_blank(), 
					axis.title.y = element_blank(), 
					legend.position = legend.place, 
					legend.background = element_rect(color = NA))
				return(print(graphic))
			} else {
				nodes$size <- net %v% size

				graphic <- ggplot() + 
					geom_segment(aes(x = x.beg, y = y.beg, xend = x.end, 
						yend = y.end), data = edges, size = 0.35, 
						color = "grey", alpha = edges$val / max(edges$val), 
						arrow = arrow(length = unit(arr.head, "cm"))) + 
					geom_point(aes(x, y, color = color, shape = shape, 
						size = size), alpha = alpha, data = nodes) +
					geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
						size = 3, color = "grey25") + 

					scale_color_brewer(palette = palette, name = color, 
						guide = col.show) + 
					scale_shape(name = shape, guide = shp.show) + 
					scale_size(name = size) + 
					scale_x_continuous(breaks = NULL) + 
					scale_y_continuous(breaks = NULL) + 
					labs(title = title) + 

					theme(panel.background = element_rect(fill = "white", 
						color = NA), 
					axis.title.x = element_blank(), 
					axis.title.y = element_blank(), 
					legend.position = legend.place, 
					legend.background = element_rect(color = NA))
				return(print(graphic))
			}
		}
	}
}
