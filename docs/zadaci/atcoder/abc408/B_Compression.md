# Задатак: B_Compression.pas

```pascal
program B_Compression;
const
    nn = 100;
var
    n, i, ai, m: int8;
    seen: array [1 .. nn] of boolean;
    c: array [1 .. nn] of int8;

begin
    readln(n);

    for ai := 1 to nn do seen[ai] := false;

    for i := 1 to n do begin
        read(ai);
        seen[ai] := true;
    end;
    readln;

    m := 0;
    for ai := 1 to nn do
        if seen[ai] then begin
            inc(m);
            c[m] := ai;
        end;

    writeln(m);
    for i := 1 to m-1 do write(c[i], ' ');
    writeln(c[m]);
end.

```
