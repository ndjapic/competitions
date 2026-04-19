# Problem: C_Kaiten_Sushi.pas

```pascal
program C_Kaiten_Sushi;
const
    nn = 200 * 1000;
var
    n, m, i, j, a, b: int32;
    mn: array [1 .. nn] of int32;

begin
    readln(n, m);

    for a := 1 to nn do mn[a] := -1;

    for i := 1 to n do begin
        read(a);
        while (a <= nn) and (mn[a] = -1) do begin
            mn[a] := i;
            inc(a);
        end;
    end;
    readln;

    for j := 1 to m do begin
        read(b);
        writeln(mn[b]);
    end;
    readln;
    writeln;
end.

```
