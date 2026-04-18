# Задатак: A_Nene_s_Game.pas

```pascal
program A_Nene_s_Game;
uses
    math;
const
    maxn = 100;
var
    ntc, tci: int16;
    n, k, q, i: int32;
    a: array [1 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(k, q);
        for i := 1 to k do read(a[i]); readln;

        for i := 1 to q do begin
            read(n);
            write(min(n, a[1]-1), ' ');
        end;
        writeln;

    end;
end.

```
