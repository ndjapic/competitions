# Problem: D_ABC_Puzzle.pas

```pascal
program D_ABC_Puzzle
const
    maxn = 5;
var
    n, i, j, k, np: int8;
    found: boolean;
    a: array [1 .. maxn, 1 .. maxn] of char;
    r, c: array [1 .. maxn] of char;
    p: array [1 .. 10] of array [1 .. 3] of int8;

procedure append(a, b, c: int8);
var
    i: int8;
begin
    inc(np);
end;

begin
    readln(n);
    for i := 1 to n do read(r[i]); readln;
    for j := 1 to n do read(c[j]); readln;

    np := 0;
    for i := 1 to n-2 do
        for j := i+1 to n-1 do
            for k := j+1 to n do begin

                inc(np);
                p[np][1] := i;
                p[np][2] := j;
                p[np][3] := k;

                inc(np);
                p[np][1] := i;
                p[np][2] := k;
                p[np][3] := j;

            end;

    found := false;
    begin
    end;

end.

```
