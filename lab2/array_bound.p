(* lab2/array_bound.p *)

var a: array 10 of integer;

begin
  a[1 * 10] := 3;
end.

(*<<
Runtime error: array bound error on line 6 in module Main
In procedure MAIN
>>*)
