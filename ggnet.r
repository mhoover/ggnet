### private functions
.set_plot_coords <- function(net, coords, layout, plot.layout, empty) {
	if(!is.null(coords)) {
		return(coords)
	} else {
		if(empty) {
			tmp <- net
			add.edges(tmp, 1, 2)
			return(coord_place(network = tmp, layout = layout, 
				plot.layout = plot.layout))
		} else {
			return(coord_place(network = net, layout = layout, 
				plot.layout = plot.layout))
		}
	}
}

.set_edge_coords <- function(net, direct, edge.val, nodes) {
	el <- as.edgelist(net)
	edges <- data.frame(nodes[el[, 1], ], nodes[el[, 2], ])
	if(!is.null(edge.val)) {
		if(!direct) {
			edge.val[upper.tri(edge.val)] <- 0
		}
		edges$val <- c(edge.val[edge.val > 0])
	} else {
		edges$val <- 2
	}
	names(edges)[1:4] <- c('x.beg', 'y.beg', 'x.end', 'y.end')
	return(edges)
}

.set_node_color <- function(net, color, gradient) {
	if(gradient) {
		return(as.numeric(net %v% color))
	} else {
		if(is.null(color)) {
			return(as.factor(1))
		} else {
			return(as.factor(net %v% color))
		}
	}
}

.set_node_name <- function(net, names) {
	if(is.null(names)) {
		return('')
	} else {
		return(net %v% names)
	}
}

.set_node_shape <- function(net, shape) {
	if(is.null(shape)) {
		return(as.factor(1))
	} else {
		return(as.factor(net %v% shape))
	}
}

.set_node_size <- function(net, size) {
	if(is.null(size)) {
		return(1)
	} else {
		return(net %v% size)
	}
}

.set_legend_attr <- function(df, attr) {
	return(ifelse(length(unique(df[, attr])) == 1, 'none', 'legend'))
}

.build_graph <- function(nodes, edges, size, alpha, palette, color, shape, 
	title, gradient, col.show, shp.show, siz.show, arr.head, legend.place, 
	empty) {
	graphic <- ggplot() 
	if(!empty) {
		graphic <- graphic + 
		    geom_segment(aes(x = x.beg, y = y.beg, xend = x.end, 
				yend = y.end), data = edges, size = 0.35, 
				color = 'grey', alpha = edges$val / max(edges$val), 
				arrow = arrow(length = unit(arr.head, 'cm')))
	}
	graphic <- graphic + 
		geom_point(aes(x, y, color = color, shape = shape, size = size), 
			alpha = alpha, data = nodes)
	if(gradient) {
		graphic <- graphic + 
			scale_color_gradient(low = 'blue', high = 'red', 
				name = color)
	} else {
		graphic <- graphic + 
			scale_color_brewer(palette = palette, name = color, 
				guide = col.show)
	}
	graphic <- graphic + 
	    geom_text(aes(x, y, label = name, vjust = -1), data = nodes, 
	    	size = 3, color = 'grey25') + 
	    scale_size(name = size, guide = siz.show) + 
	    scale_shape(name = shape, guide = shp.show) + 
		scale_x_continuous(breaks = NULL) + 
		scale_y_continuous(breaks = NULL) + 
		labs(title = title) + 
		theme(panel.background = element_rect(fill = 'white', 
			color = NA), axis.title.x = element_blank(), 
			axis.title.y = element_blank(), 
			legend.position = legend.place, 
			legend.background = element_rect(color = NA))
	return(graphic)
}

### public functions
coord_place <- function(network, layout = 'fruchtermanreingold', 
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
	graph.layout <- match.fun(paste('gplot.layout.', layout, sep = ''))

	# create data frame of node coordinates (based on graph layout algorithm)
	nodes <- data.frame(graph.layout(network, plot.layout))
	names(nodes) <- c('x', 'y')
	return(nodes)
}

ggnet <- function(net, direct = FALSE, color = NULL, names = NULL, 
	shape = NULL, size = NULL, edge.val = NULL, title = NULL, legend = FALSE, 
	coords = NULL, layout = 'fruchtermanreingold', plot.layout = NULL, 
	palette = 'Set1', gradient = FALSE, alpha = 1) {

	# load dependencies
	require(ggplot2, quietly = TRUE)
	require(grid, quietly = TRUE)
	require(statnet, quietly = TRUE)

	# determine if graph is empty
	empty <- sum(as.matrix(net)) == 0

	# determine plot coordinates
	nodes <- .set_plot_coords(net, coords, layout, plot.layout, empty)

	# create data frame of edge coordinates from node to node
	edges <- .set_edge_coords(net, direct, edge.val, nodes)

	# determine node attributes
	nodes$color <- .set_node_color(net, color, gradient)
	nodes$name <- .set_node_name(net, names)
	nodes$shape <- .set_node_shape(net, shape)
	nodes$size <- .set_node_size(net, size)

	# determine arrows for directed network
	arr.head <- ifelse(direct == TRUE, .2, 0)

	# determine legend 
	legend.place <- ifelse(legend == TRUE, 'right', 'none')

	# set legend attributes
	col.show <- .set_legend_attr(nodes, 'color')
	shp.show <- .set_legend_attr(nodes, 'shape')
	siz.show <- .set_legend_attr(nodes, 'size')
	
	# create network plot
	plot <- .build_graph(nodes, edges, size, alpha, palette, color, shape, 
		title, gradient, col.show, shp.show, siz.show, arr.head, legend.place, 
		empty)

	# return graph
	return(print(plot))
}
