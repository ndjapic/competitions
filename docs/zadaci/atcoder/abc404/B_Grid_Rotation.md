# Задатак: B_Grid_Rotation.pas

```pascal
program B_Grid_Rotation;
{$MODE DELPHI}
uses
    math;
const
    nn = 100;
    inf = nn * nn;
var
    n, i, j, e: int8;
    ans, c: int16;
    s, t, w: array [1 .. nn] of string;

begin
    readln(n);

    for i := 1 to n do readln(s[i]);
    for i := 1 to n do readln(t[i]);
    for i := 1 to n do setlength(w[i], n);

    ans := inf;
    for e := 0 to 3 do begin

        c := e;
        for i := 1 to n do
            for j := 1 to n do begin
                if s[i][j] <> t[i][j] then inc(c);
                w[j][n+1-i] := s[i][j];
            end;

        ans := min(ans, c);

        for i := 1 to n do s[i] := w[i];

    end;

    writeln(ans);
end.

```
