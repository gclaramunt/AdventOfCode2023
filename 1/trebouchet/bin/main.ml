open Trebouchet




let rec process_lines acc =
  try
    let line = read_line ()
  in let code = calibration_value ( String.to_seq line ) in
    process_lines (code ::acc)  (* Recursively call to read the next line *)
  with
  | End_of_file -> acc  (* End of input reached *)


let () = let total_sum = List.fold_left (+) 0 (process_lines []) in Printf.printf "Sum of all calibration numbers: %d\n" total_sum 