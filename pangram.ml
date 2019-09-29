let rec checkbyascii(inputstr:string)(cha:char):string=
  let asc=int_of_char cha in
  let newchar=char_of_int (asc+1) in
  if asc<=122 then
  match String.contains inputstr cha with
  |false -> "false"
  |true ->checkbyascii inputstr newchar
  else
  "true";;

let rec read_line (txtinput:in_channel):string list=
  match input_line txtinput with
  |l ->checkbyascii l 'a' :: read_line txtinput
  |exception End_of_file -> [];;

let rec write_line(txtoutput:out_channel)(newlist:string list): unit=
  match newlist with
    | l::ls->let _=Printf.fprintf txtoutput "%s\n" l in write_line txtoutput ls
    | []->();;
  
let pangram((input,output): string*string):unit=
  let ic=open_in input in
  let oc=open_out output in
  let stri_list=read_line ic in
  let _=write_line oc stri_list in
  let _=close_in ic in
  let _=close_out oc in 
  ();;
