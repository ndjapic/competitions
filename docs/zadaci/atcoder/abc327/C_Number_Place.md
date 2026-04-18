# Задатак: C_Number_Place.pas

```pascal
program C_Number_Place;
var
    i, j: int8;
    ans: boolean;
    a: array [1 .. 9, 1 .. 9] of int8;
    c: array [1 .. 9] of int8;

procedure rec(i1, i2, j1, j2: int8);
var
    i, j, k: int32;
begin
    for k := 1 to 9 do c[k] := 0;
    for i := i1 to i2 do
        for j := j1 to j2 do begin
            inc(c[a[i, j]]);
            ans := ans and (c[a[i, j]] < 2);
        end;
end;

begin
    for i := 1 to 9 do begin
        for j := 1 to 9 do read(a[i, j]);
        readln;
    end;

    ans := true;
    for i := 1 to 9 do rec(i, i, 1, 9);
    for j := 1 to 9 do rec(1, 9, j, j);

    for i := 1 to 3 do
        for j := 1 to 3 do
            rec(3*i-2, 3*i, 3*j-2, 3*j);

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.

```
