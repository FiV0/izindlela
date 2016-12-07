let html_code =
"<!DOCTYPE html>
<html>

  <head>
    <meta charset=utf-8 />
    <title></title>
    <link href=\"https://api.tiles.mapbox.com/mapbox.js/v2.1.4/mapbox.css\" 
	  rel=\"stylesheet\"/>
    <script src=\"https://api.tiles.mapbox.com/mapbox.js/v2.1.4/mapbox.js\">
    </script>
    <style>
      body { margin:0; padding:0; }
      html, body, #map { height: 100%; }
    </style>
  </head>

  <body>
    <div id=\"map\"></div>  
  </body>

</html>"

let js_init (maxlat, minlat, maxlon, minlon) = Printf.sprintf
"var maxlat = %f;
var minlat = %f; 
var maxlon = %f;
var minlon = %f; 

L.mapbox.accessToken = 
    \"pk.eyJ1Ijoid2FzZSIsImEiOiJpZUZ6WGZNIn0.emLBxZM0GZJ0krfJOz_mYw\";

var map = L.mapbox.map(\"map\", \"wase.kd2b8p50\", {attributionControl: false})
    .fitBounds([[minlat, minlon], [maxlat, maxlon]]);

var credits = L.control.attribution()
    .setPrefix(\"&copy; Mapbox &copy; OpenStreetMap\")
    .addTo(map);

var from_marker = L.marker([0, 0], 
                           {clickable: false, keyboard: false, opacity:0.0})
    .addTo(map);
var to_marker = L.marker([0, 0], 
                         {clickable: false, keyboard: false, opacity:0.0})
    .addTo(map);

var path = L.polyline([], {color: \"red\"})
    .addTo(map);

var visited = L.layerGroup([]);"  
maxlat minlat maxlon minlon

let js_move (maxlat, minlat, maxlon, minlon) = Printf.sprintf
    "map.fitBounds([[%f, %f], [%f, %f]]);" minlat minlon maxlat maxlon

let js_set_marker (lat, lon) way = 
  Printf.sprintf "map.setView([%f,%f], 15, {animate: true});" lat lon 
  ^
  begin match way with
    | `FROM -> 
      "from_marker.setOpacity(1.0);from" 
    | `TO ->  
      "to_marker.setOpacity(1.0);to"
  end
  ^ 
  Printf.sprintf "_marker.setLatLng([%f,%f]);" lat lon  

let js_draw_path =
  "map.fitBounds(path.getBounds(), {padding: [20,20]});"

let js_clear_path = "path.setLatLngs([]);"

let js_add_waypoint (lat, lon) = 
  Printf.sprintf "path.addLatLng([%f, %f]);" lat lon

let js_clear_visited = "visited.clearLayers();"
 
let js_toggle_visited visible = 
  "map." ^ (if visible then "addLayer" else "removeLayer") ^ "(visited);" 

let js_add_visited_point (lat, lon) = 
  Printf.sprintf "visited.addLayer(L.circleMarker([%f, %f], {radius: 1}));" lat lon
