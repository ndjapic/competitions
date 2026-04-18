# Задатак: B_Improve_Inversions.pas

```pascal
program B_Improve_Inversions;
{$mode objfpc}{$H+}{$J-}
const
    nn = 250 * 1000;
    prime = 1000 * 1000 * 1000 + 7;
var
    n, m, k, i, ans: int32;
    s: string;
    a, b: array [1 .. nn] of int32;

begin
    readln(n);
    readln(s);

    a[1] := 1;
    m := 1;
    for i := 2 to n do begin
        if s[i] <> s[i-1] then begin
            inc(m);
            a[m] := 0;
        end;
        inc(a[m]);
    end;

    inc(a[1]);
    inc(a[m]);

    b[1] := 0;
    k := 1;
    for i := 1 to m do
        if a[i] = 1 then
            inc(b[k])
        else if b[k] > 0 then begin
            inc(k);
            b[k] := 0;
        end;

    ans := 1;
    for i := 1 to k do
        ans := (b[i] + 3) div 2 * ans mod prime;

    writeln(ans);
end.

```
