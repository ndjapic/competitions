# Problem: B_Couples.pas

```pascal
program B_Couples;
{$mode objfpc}{$H+}{$J-}
const
    nn = 200;
var
    n, i, ans: int16;
    a: array [1 .. nn] of int8;
    seen: array [1 .. nn] of boolean;

begin
    readln(n);

    for i := 1 to n do seen[i] := false;

    ans := 0;
    read(a[1]);
    read(a[2]);

    for i := 3 to 2*n do begin
        read(a[i]);
        if not seen[a[i]] and (a[i-2] = a[i]) then begin
            inc(ans);
            seen[a[i]] := true;
        end;
    end;
    readln;

    writeln(ans);
end.

```
