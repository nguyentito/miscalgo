let pokeperm = [|101;50;32;56;3;19;22;16;119;135;149;92;77;134;62;12;49;60;33;55;2;88;71;146;34;83;63;39;99;111;36;40;41 
;57;52;95;74;78;82;143;94;114;118;21;110;86;130;80;140;53;46;116;20;81;64;70;132;91;10;27;1;4;76;25;65;84;48;67;115;97;125;136;9;61;43;96;26;31;69;121;11;105;14;35;85;117;45;98;108;128;13;29;66;90;137;107;144 
;124;30;15;93;7;113;47;102;89;87;17;131;37;112;6;68;127;109;51;42;120;129;38;23;139;123;72;148;126;28;106;54;59;133;103;141;145;147;58;122;104;75;100;44;5;8;150;73;142;138;79;18;24|]


let decompo perm =
  let perm = Array.map (fun x -> x - 1) perm in
  let n = Array.length perm in
  let stack = ref [] and cycle = ref [] in
  let visited = Array.make n false in
  for i = 0 to n - 1 do
    if not visited.(i) then (
      let x = ref i in
      cycle := [i];
      visited.(i) <- true;
      while perm.(!x) <> i do
        x := perm.(!x);
        cycle := !x :: !cycle;
        visited.(!x) <- true
      done;
      stack := (List.rev !cycle) :: (!stack)
    )
  done;
  List.rev_map (List.map ((+) 1)) !stack
  
