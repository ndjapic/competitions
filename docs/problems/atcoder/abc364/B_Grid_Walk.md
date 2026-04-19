# Problem: B_Grid_Walk.pas

```pascal
program B_Grid_Walk;
{$mode objfpc}{$H+}{$J-}
const
    nn = 50;
var
    h, w, si, sj, i, k: int8;
    c: array [1 .. nn] of string;
    x: string;

procedure mv(i, j: int8);
begin
    if (1 <= i) and (i <= h) then
        if (1 <= j) and (j <= w) then
            if c[i][j] = '.' then begin
                si := i;
                sj := j;
            end;
end;

begin
    readln(h, w);
    readln(si, sj);
    for i := 1 to h do readln(c[i]);
    readln(x);

    for k := 1 to length(x) do
        if x[k] = 'L' then
            mv(si, sj-1)
        else if x[k] = 'R' then
            mv(si, sj+1)
        else if x[k] = 'U' then
            mv(si-1, sj)
        else if x[k] = 'D' then
            mv(si+1, sj);

    writeln(si, ' ', sj);
end.

```
