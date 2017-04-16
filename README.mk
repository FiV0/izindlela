
## Izindlela ##
	Izindlela is an ocaml program which does shortest path routing for different
	transportation modes of a map in osm format with visualization.

### Installation of third party libaries via opam: ###
	> opam install xml-light ocamlnet lablgtk

### Installation of lablwebkit : ###
	> sudo apt-get install libwebkitgtk-dev
	> opam install oasis
	* in _lib/lablwebkit-1.2.8.2 (having decompressed the archive) :
	   > ocaml setup.ml -configure
	   > ocaml setup.ml -build
	   > oasis setup
	   > ocaml setup.ml -install

### Compilation : ###
	> make

### Usage : ###
	> ./main.native [OSM_filename]
	* Via the GUI one can charge a file through [File] -> [Load OSM map]
	  (in case OSM file was not specified).
	  The interface has:
	  - 2 text fields for entering the addresses
		  (validation of the request via [RET] or with the associated buttons).
		  Two flags will be displayed.
		- Selectors allow to specify the algorithm used for exploring the search
			space. An error message is shown in case start or destination are not in the limits
			of the loaded OSM map.
			The route will displayed on the map and a selector also gives the possibility to
		 	show the visited search space for the chosen algorithm.
	* the option -s starts a command line interface

### simple dijkstra ###

![image failed to load][./latex/fig/dijkstra.png]

### A* with euclidean distance as lower bound + a second layer for search ###

![image failed to load][./latex/fig/euclidien_hl.png]
