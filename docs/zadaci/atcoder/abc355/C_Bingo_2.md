# Задатак: C_Bingo_2.pas

```pascal
program C_Bingo_2;
uses
    math;
const
    nn = 2000;
    tt = 200 * 1000;
var
    n, t, i, k, major, minor: int32;
    found: boolean;
    a: array [1 .. tt] of int32;
    row, col: array [1 .. nn] of int32;

function bingo(x: int32): boolean;
var
    i, j: int32;
begin
    dec(x);
    i := x div n + 1;
    j := x mod n + 1;
    dec(row[i]);
    dec(col[j]);
    if i = j then dec(major);
    if i+j = n+1 then dec(minor);
    bingo := (row[i] = 0) or (col[j] = 0) or (major = 0) or (minor = 0);
end;

begin
    readln(n, t);

    for k := 1 to t do read(a[k]);
    readln;

    for i := 1 to n do begin
        row[i] := n;
        col[i] := n;
        major := n;
        minor := n;
    end;

    k := 1;
    found := false;
    while not found and (k <= t) do begin
        found := bingo(a[k]);
        inc(k);
    end;

    dec(k);
    if not found then k := -1;
    writeln(k);
end.

```
