# ggnet
ggnet was created to do network visualization using the `ggplot2` package in R. It is intended to create prettier network visualizations than base R graphics, with an array of color themes and attributes, such as node size, color, shape, and directed/undirected edges. It is a work-in-progress. 

# Usage
Currently, ggnet consists of two R functions, but will (hopefully) be turned into an actual package at some point. To start, the actual `ggnet` function requires input data to be a network-class object from the `statnet` package. Most types of network data, including edgelists and matrices, can be converted to an network class object with little trouble.

```r
library(statnet)

d <- data.frame(sender = c(rep(1, 4), rep(2, 3), rep(3, 4), rep(4, 1)), 
    receiver = c(sample(2:5, 4), sample(c(1, 3:5), 3), sample(c(1:2, 4:5), 
    4), sample(c(1:3, 5), 1)))

net <- network(d)
```

At a minimum, the `ggnet` function only needs a network object as input data; the function defaults to an undirected network.

```r
ggnet(net)
```

![Basic network graph](https://github.com/mhoover/ggnet/blob/master/images/basic_graph.jpg 'Basic network graph')

However, there are a robust set of options to improve the aesthetics of the network visualization. These include:
* `direct`: Boolean value indicating whether or not the network is directed or not.
  * Default is FALSE.
  * If TRUE, arrows will be added to the edges indicating sender/receiver.
* `color`: Vertex attribute from network-class object (indicated in quotations, e.g., 'age'). 
  * Default is NULL. 
  * Should be inputted as a string. 
  * Indicates nodes should be colored based on the age attribute of the node.
* `names`: Vertex attribute from network-class object (indicated in quotations, e.g., 'vertex.names'). 
  * Default is NULL. 
  * Should be inputted as a string. 
  * Indicates nodes should be labeled with the value of the vertex.names attribute (appears as a label directly above the node).
* `shape`: Vertex attribute from network-class object (indicated in quotations, e.g., 'gender'). 
  * Default is NULL. 
  * Should be inputted as a string. 
  * Indicates nodes should be shaped (i.e., circle, triangle, square, etc.) based on the gender attribute of the node.
* `size`: Vertex attribute from network-class object (indicated in quotations, e.g., 'test.score'). 
  * Default is NULL. 
  * Should be inputted as a string.
  * Indicates nodes should be sized (i.e., smaller/larger) based on the test.score attribute of the node.
* `edge.val`: Attribute to make edges differentially transparent, based on value. 
  * Default is NULL. 
  * This parameter should be read into the function as a matrix of numeric values 
  * It will convert the edge transparency from highly transparent (small value in matrix) to opaque (high value in matrix).
* `title`: Provide a title to the visualization. 
  * Default is NULL. 
  * Should be inputted as a string.
* `legend`: Boolean value to display legend of node/edge attributes used.
  * Default is FALSE.
  * If set to TRUE and with no attributes used, then it is ignored.
* `coords`: Sets placement of nodes in a specified location.
  * Default is NULL.
  * This parameter should be a data frame of x/y coordinates.
  * Graph algorithms use force-directed placement of nodes, starting with a random value. To achieve consistent placement of nodes for the same networks over time, then specifying `coords` is advised; otherwise, each time the graph is produced, nodes will be in a different place in space.
  * Specified, reusable coordinates for a network can be created with the `coord.place` function.
* `layout`: The type of graph layout used in visualization.
  * Default is 'fruchtermanreingold'.
  * Other (common) options include 'kamadakawai', 'rmds', and 'eigen'.
* `plot.layout`: Default is NULL; only used for certain algorithm layouts. See `gplot.layout` for more details.
* `palette`: Identifies the `ggplot2` palatte to use when plotting.
  * Default is 'Set1'.
  * See `ggplot2` color theme options for more information.
* `gradient`: Boolean value for whether or not the `color` attribute should be treated continuously (i.e., as a gradient) or categorically.
  * Default is FALSE.
  * If TRUE, color attribute will be displayed as a gradient on red/blue spectrum.
* `alpha`: Numeric value indicating the level of transparency in color.
  * Default is 1.
  * Ranges from 0 (fully transparent) to 1 (opaque).

```r
ggnet(net, direct = TRUE, title = 'Network graph with attributes', 
    names = 'vertex.names')
```

![Network graph with attributes](https://github.com/mhoover/ggnet/blob/master/images/attribute_graph.jpg 'Network graph with attributes')

# Example data
An example network is included with various node (vertex) attributes that can be used to modify the visualization. It is a network-class object in R and will require the `statnet` (or `network`) package to be able to properly access its data. Working with network-class objects can be a little tricky, so please refer to [the manual](https://cran.r-project.org/web/packages/network/network.pdf) on CRAN.

# Known issues
1. With directed graphs, the arrow ends on edges are parially obscured/hidden by the nodes. 
2. Input data is limited to network-class objects at the current time.

# Additional information
The underlying graphing capabilities make extensive use of `ggplot2` therefore, knowledge of `ggplot2` is helpful in terms of developing new features or tweaking existing ones. Help on `ggplot2` is available [here](http://docs.ggplot2.org/current). For more information, contact matthew.a.hoover at gmail.com.