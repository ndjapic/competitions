program A_A_B_Again.pas;
{$mode objfpc}{$H+}{$J-}
(* https://codeforces.com/contest/1997/problem/A *)
uses
    math;
var
    ntc, tci: int16;
    n: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        writeln(n div 10 + n mod 10);

    end;
end.
