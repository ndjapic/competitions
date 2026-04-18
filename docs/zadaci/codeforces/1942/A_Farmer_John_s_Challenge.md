# Задатак: A_Farmer_John_s_Challenge.pas

```pascal
program A_Farmer_John_s_Challenge;
const
    maxn = 1000;
var
    ntc, tci, n, k, i: int16;
    a: array [1 .. maxn] of int8;

begin
    for i := 1 to maxn do a[i] := 1;
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        if k = 1 then
            a[n] := 2
        else if k = n then
            a[n] := 1;

        if (k = 1) or (k = n) then begin
            for i := 1 to n-1 do write(a[i], ' ');
            writeln(a[n]);
            a[n] := 1;
        end else
            writeln(-1);

    end;
end.

```
