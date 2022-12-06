let line_opt channel = 
  try Some (input_line channel)
  with End_of_file -> None
  