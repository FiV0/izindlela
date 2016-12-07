let () =
  if Array.length Sys.argv = 1 
  then
    GUI.init None
  else if Array.length Sys.argv > 1
  then 
    let filename = Array.get Sys.argv 1 in
    if Sys.file_exists filename 
    then 
      if Array.length Sys.argv > 2
      then 
        let simple_ui = Array.get Sys.argv 2 in
        if simple_ui = "-s" 
        then
          SimpleUI.init filename
        else
          print_string ("Bad argument. Try -s.")
      else
        GUI.init (Some filename)
    else 
      print_string (String.concat filename ["File: ";" does not exist\n"])
