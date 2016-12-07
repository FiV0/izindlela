open Http_client
open Xml

let get_osm_data request =
  let query = "http://nominatim.openstreetmap.org/search?q=" ^
              Str.global_replace (Str.regexp " ") "+" request ^ 
              "&format=xml&limit=1" 
  in
  (*let x = new get query in 
  let p = new pipeline in
  p # add x;
  p # run();
  match x # status with
  | `Successful -> 
    let str_data = x # response_body # value in 
    let xml_data = parse_string str_data in
    begin 
      match children xml_data with
      | [Element ("place", attr, _)] ->
        begin 
          try 
            let lat = float_of_string (List.assoc "lat" attr) in 
            let lon = float_of_string (List.assoc "lon" attr) in 
            lat, lon
          with 
          | Not_found -> failwith "List.assoc fail"
          | Failure "float_of_string" -> failwith "float_of_string fail"
          | Failure "int_of_string" -> failwith "int_of_string fail"          
        end 
      | _ -> failwith "xml response fail"
    end 
  | `Http_protocol_error _|`Redirection|`Server_error|`Unserved|`Client_error 
    -> failwith "http fail"*)
  let str_data = Convenience.http_get query in 
  let xml_data = parse_string str_data in 
  match children xml_data with
  | [Element ("place", attr, _)] ->
    begin 
      try 
        let lat = float_of_string (List.assoc "lat" attr) in 
        let lon = float_of_string (List.assoc "lon" attr) in 
        Some (lat, lon)
      with 
      | Not_found 
      | Failure "float_of_string" 
      | Failure "int_of_string" -> None         
    end 
  | _ -> None
