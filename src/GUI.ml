open ParsedMap

let _ = GMain.init ()

let window = GWindow.window 
    ~title:"Izindlela" 
    ~height:600 
    ~width:800 
    ~border_width:5 () 

let main_frame = GPack.vbox 
    ~spacing:10 
    ~packing:window#add () 

let menubar = GMenu.menu_bar ~packing:main_frame#add () 

let search_from_field = GEdit.entry () 
let search_to_field = GEdit.entry () 
let search_from_button = GButton.button () 
let search_to_button = GButton.button () 

let search_frame =
  let search_frame = GPack.table 
      ~rows:2 
      ~columns:3 
      ~packing:main_frame#add () 
  in
  let from_lbl = GMisc.label ~text:"From: " () in
  let to_lbl = GMisc.label ~text:"To: " () in
  search_frame#attach ~left:0 ~top:0 (from_lbl#coerce);
  search_frame#attach ~left:0 ~top:1 (to_lbl#coerce);

  search_frame#attach ~expand:`BOTH ~left:1 ~top:0 (search_from_field#coerce);
  search_frame#attach ~left:1 ~top:1 (search_to_field#coerce);

  ignore (GMisc.image ~stock:`FIND ~icon_size:`SMALL_TOOLBAR 
            ~packing:search_from_button#add ());
  ignore (GMisc.image ~stock:`FIND ~icon_size:`SMALL_TOOLBAR 
            ~packing:search_to_button#add ());
  search_frame#attach ~left:2 ~top:0 (search_from_button#coerce);
  search_frame#attach ~left:2 ~top:1 (search_to_button#coerce);
  search_frame

let nav_frame = GPack.hbox
    ~spacing:10 
    ~packing:main_frame#add ()

let left_pane = GPack.vbox 
    ~spacing:5 
    ~width:130
    ~packing:nav_frame#add ()

let transport_frame = GPack.vbox 
    ~packing:left_pane#pack ()
let transport_label = GMisc.label ~xalign:0.0 ~text:"Transport:" 
    ~packing:transport_frame#pack ()
let car = GButton.radio_button 
    ~label:"Car"
    ~active:true
    ~packing:transport_frame#pack () 
let cycle = GButton.radio_button 
    ~group:car#group 
    ~label:"Bike"
    ~packing:transport_frame#pack () 
let pedestrian = GButton.radio_button
    ~group:car#group 
    ~label:"Pedestrian" 
    ~packing:transport_frame#pack ()
let separator = GMisc.separator `HORIZONTAL ~packing:left_pane#pack ()
let priority_frame = GPack.vbox 
    ~packing:left_pane#pack ()
let priority_label = GMisc.label ~xalign:0.0 ~text:"Priority:" 
    ~packing:priority_frame#pack ()
let fast = GButton.radio_button 
    ~label:"Fastest"
    ~active:true
    ~packing:priority_frame#pack () 
let short = GButton.radio_button 
    ~group:fast#group 
    ~label:"Shortest"
    ~packing:priority_frame#pack () 
let suitable = GButton.radio_button
    ~group:fast#group 
    ~label:"Nicest" 
    ~show:false
    ~packing:priority_frame#pack () 

let separator = GMisc.separator `HORIZONTAL ~packing:left_pane#pack ()
let algo_frame = GPack.vbox 
    ~packing:left_pane#pack ()
let algo_label = GMisc.label ~xalign:0.0 ~text:"Algorithm:" 
    ~packing:algo_frame#pack ()
let dijkstra = GButton.radio_button 
    ~label:"Dijkstra"
    ~active:true
    ~packing:algo_frame#pack () 
let a_star = GButton.radio_button 
    ~group:dijkstra#group 
    ~label:"A* euclidian"
    ~packing:algo_frame#pack () 
let landmarks = GButton.radio_button 
    ~group:dijkstra#group 
    ~label:"A* landmarks"
    ~packing:algo_frame#pack () 
let highlayer = GButton.check_button
    ~label:"Shortcut graph"
    ~packing:algo_frame#pack ()

let separator = GMisc.separator `HORIZONTAL ~packing:left_pane#pack ()
let display_visited = GButton.check_button
    ~label:"Display visited\nsearch space"
    ~packing:left_pane#pack ()

let separator = GMisc.separator `HORIZONTAL ~packing:left_pane#pack () 
let calculate_button = GButton.button 
    ~label:"GO !" 
    ~packing:left_pane#pack () 
let separator = GMisc.separator `HORIZONTAL ~packing:left_pane#pack ()

let infos = GPack.hbox ~spacing:10 ~packing:left_pane#pack () 
let infos_labels = GPack.vbox ~spacing:5 ~packing:infos#pack () 
let infos_values = GPack.vbox ~spacing:5 ~packing:infos#pack () 
let dist_label = GMisc.label ~xalign:0.0 ~text:"Distance:" 
    ~packing:infos_labels#pack () 
let time_label = GMisc.label ~xalign:0.0 ~text:"Estimation:" 
    ~packing:infos_labels#pack () 
let runtime_label = GMisc.label ~xalign:0.0 ~text:"Search time:" 
    ~packing:infos_labels#pack () 
let dist_value = GMisc.label ~xalign:0.0 ~packing:infos_values#pack () 
let time_value = GMisc.label ~xalign:0.0 ~packing:infos_values#pack () 
let runtime_value = GMisc.label ~xalign:0.0 ~packing:infos_values#pack () 

let web_view = 
  let web_view = GWebView.web_view 
      ~packing:nav_frame#add () 
  in
  web_view#settings#set_enable_default_context_menu false;     
  web_view

let progressbar = GRange.progress_bar ~packing:main_frame#add ()

let handle_status = function 
  | `PROVISIONAL -> progressbar#set_fraction 0.25
  | `COMMITTED -> progressbar#set_fraction 0.5 
  | `FIRST_VISUALLY_NON_EMPTY_LAYOUT -> progressbar#set_fraction 0.75
  | `FINISHED ->
    progressbar#set_fraction 1.;
    let bounds = Application.get_bounds () in
    web_view#execute_script (WebCode.js_init bounds);
    progressbar#misc#hide ()
  | `FAILED -> 
    progressbar#misc#hide ()

let hide_dlg dlg = fun _ -> dlg#misc#hide ()

let notfound_dialog = 
  let notfound_dialog = GWindow.message_dialog
      ~message:"<big><b>Endpoint not found.</b></big>\
                \n\nPlease write a proper address."
      ~use_markup:true
      ~parent:window
      ~destroy_with_parent:true
      ~message_type:`ERROR
      ~position:`CENTER_ON_PARENT
      ~buttons:GWindow.Buttons.ok ()
  in 
  ignore (notfound_dialog#connect#response 
            ~callback:(hide_dlg notfound_dialog));
  notfound_dialog

let choose_dialog = 
  let choose_dialog = GWindow.message_dialog
      ~message:"<big><b>No specified endpoints.</b></big>\
                \n\nPlease choose endpoints."
      ~use_markup:true
      ~parent:window
      ~destroy_with_parent:true
      ~message_type:`ERROR
      ~position:`CENTER_ON_PARENT
      ~buttons:GWindow.Buttons.ok ()
  in 
  ignore (choose_dialog#connect#response 
            ~callback:(hide_dlg choose_dialog));
  choose_dialog

let outbound_dialog = 
  let outbound_dialog = GWindow.message_dialog
      ~message:"<big><b>Outbound endpoint.</b></big>\
                \n\nPlease choose a proper endpoint."
      ~use_markup:true
      ~parent:window
      ~destroy_with_parent:true
      ~message_type:`ERROR
      ~position:`CENTER_ON_PARENT
      ~buttons:GWindow.Buttons.ok ()
  in 
  ignore (outbound_dialog#connect#response 
            ~callback:(hide_dlg outbound_dialog));
  outbound_dialog

let search_point field way () = 
  let address = field#text in
  let latlon = Application.get_point address in
  match latlon with
  | Some latlon -> 
    Application.update_endpoint latlon way;
    web_view#execute_script WebCode.js_clear_path;
    web_view#execute_script WebCode.js_clear_visited;
    web_view#execute_script (WebCode.js_set_marker latlon way);
    dist_value#set_text "";
    time_value#set_text "" 
  | None -> notfound_dialog#show ()

let format_distance d =
  if (d > 1.) 
  then 
    let d = ceil (d *. 10.) /. 10. in
    (string_of_float d) ^ "km"
  else 
    let d = int_of_float (ceil (d *. 1000.)) in
    (string_of_int d) ^ "m"

let format_time t =
  let t = Unix.gmtime t in
  let h = t.Unix.tm_hour in
  let m = t.Unix.tm_min in
  if h = 0 
  then if m = 0 
    then "<1min" 
    else (string_of_int m) ^ "min" 
  else (string_of_int h) ^ "h" ^ (string_of_int m) ^ "min"

let format_runtime t =
  if t <= 60.
  then Printf.sprintf "%.3fs" t
  else format_time t

let calculate () = 
  match Application.calculate () with
  | Some (run_time, time, dist, path, visited) ->
    web_view#execute_script WebCode.js_clear_path;
    web_view#execute_script WebCode.js_clear_visited;
    List.iter (fun id -> 
        let latlon = Application.find_point id in
        web_view#execute_script (WebCode.js_add_waypoint latlon);
      ) path;
    List.iter (fun id -> 
        let latlon = Application.find_point id in
        web_view#execute_script (WebCode.js_add_visited_point latlon);
      ) visited;
    web_view#execute_script WebCode.js_draw_path;
    let time = time *. 3600. in
    dist_value#set_text (format_distance dist);
    time_value#set_text (format_time time);
    runtime_value#set_text (format_runtime run_time)  
  | None -> 
    if search_from_field#text = "" && search_to_field#text = "" 
    then choose_dialog#show ()
    else outbound_dialog#show ()

let update_transport () =
  if not fast#active then fast#set_active true;
  let transport = 
    if car#active 
    then begin
      fast#misc#show ();
      short#misc#show ();
      suitable#misc#hide ();
      ParsedMap.Car end
    else if cycle#active 
    then begin
      fast#misc#show ();
      short#misc#hide ();
      suitable#misc#show ();
      ParsedMap.Bike end
    else begin
      fast#misc#show ();
      short#misc#hide ();
      suitable#misc#show ();
      ParsedMap.Foot end
  in
  Application.update_transport transport

let update_priority () =
  let priority =
    if fast#active then ParsedMap.Fast 
    else if short#active then ParsedMap.Short 
    else ParsedMap.Suitable
  in 
  Application.update_priority priority

let update_algo () =
  let algo = if dijkstra#active 
    then Application.Dijkstra 
    else if a_star#active then Application.A_Star_E
    else Application.A_Star_L
  in
  Application.update_algo algo

let toggle_highlayer () =
  Application.update_use_highlayer highlayer#active 

let toggle_visited () =
  web_view#execute_script (WebCode.js_toggle_visited display_visited#active) 

let move_map () =
  let bounds = Application.get_bounds () in
  web_view#execute_script (WebCode.js_move bounds)

let load_dialog_response 
    (dlg:( [`CANCEL | `DELETE_EVENT | `OPEN] GWindow.file_chooser_dialog)) = 
  function
  | `CANCEL | `DELETE_EVENT -> dlg#misc#hide () 
  | `OPEN -> 
    let s = dlg#filename in 
    Application.load_map s; 
    move_map (); 
    hide_dlg dlg ()

let load_dialog = 
  let load_dialog = GWindow.file_chooser_dialog
      ~title:"Load OSM map"
      ~action:`OPEN
      ~parent:window
      ~position:`CENTER_ON_PARENT
      ~destroy_with_parent:true ()
  in
  load_dialog#add_button_stock `CANCEL `CANCEL;
  load_dialog#add_select_button_stock `OPEN `OPEN;
  ignore (load_dialog#connect#response 
            ~callback:(load_dialog_response load_dialog));
  load_dialog

let about_dialog =
  let about_dialog = GWindow.about_dialog 
      ~authors:["Lélio Brun"; "Finn Völkel"]
      ~name:"Izindlela"
      ~version:"1.0"
      ~position:`CENTER_ON_PARENT
      ~parent:window
      ~destroy_with_parent:true ()
  in 
  ignore (about_dialog#connect#response 
            ~callback:(hide_dlg about_dialog));
  about_dialog

let file_entries = [
  `I ("Load OSM map", load_dialog#show);
  `S;
  `I ("Quit", GMain.quit)
]

let help_entries = [
  `I ("About...", about_dialog#show);
]

let entries = [
  "File", file_entries;
  "Help", help_entries
]

let create_menu menubar label =
  let item = GMenu.menu_item ~label ~packing:menubar#append () in
  GMenu.menu ~packing:item#set_submenu ()

let _ = List.iter (fun (l, entries) -> 
    let menu = create_menu menubar l in 
    GToolbox.build_menu menu ~entries) entries

let init s_opt = 
  Application.load_map s_opt;
  ignore (window#connect#destroy ~callback:GMain.quit);
  ignore (search_from_field#connect#activate 
            ~callback:(search_point search_from_field `FROM));
  ignore (search_from_button#connect#clicked 
            ~callback:(search_point search_from_field `FROM));
  ignore (search_to_field#connect#activate 
            ~callback:(search_point search_to_field `TO));  
  ignore (search_to_button#connect#clicked 
            ~callback:(search_point search_to_field `TO));

  ignore (car#connect#toggled ~callback:update_transport);
  ignore (cycle#connect#toggled ~callback:update_transport);
  ignore (pedestrian#connect#toggled ~callback:update_transport);
  
  ignore (fast#connect#toggled ~callback:update_priority);
  ignore (short#connect#toggled ~callback:update_priority);
  ignore (suitable#connect#toggled ~callback:update_priority);

  ignore (dijkstra#connect#toggled ~callback:update_algo);
  ignore (a_star#connect#toggled ~callback:update_algo);
  ignore (landmarks#connect#toggled ~callback:update_algo);
  
  ignore (highlayer#connect#toggled ~callback:toggle_highlayer);
  ignore (display_visited#connect#toggled ~callback:toggle_visited);

  ignore (calculate_button#connect#clicked ~callback:calculate); 
  ignore (web_view#connect#notify_load_status 
    ~callback:handle_status);

  web_view#load_string WebCode.html_code "text/html" "UTF-8" "";

  window#show ();  
  GMain.main ()
