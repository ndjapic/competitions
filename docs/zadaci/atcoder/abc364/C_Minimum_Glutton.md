# Задатак: C_Minimum_Glutton.pas

```pascal
program C_Minimum_Glutton;
{$mode objfpc}{$H+}{$J-}
uses
    math, Generics.Defaults, Generics.Collections;
type
    intList = specialize TList<int32>;
var
    n, i, j, v: int32;
    x, y: int64;
    a, b: intList;

function intCompare(constref Left, Right: int32): int32;
begin
    Result := -Left + Right;
end;

begin
    readln(n, x, y);

    a := intList.Create();
    for i := 1 to n do begin
        read(v);
        a.add(v);
    end;
    readln;
    a.Sort(specialize TComparer<int32>.Construct(@intCompare));

    i := 0;
    while (i < n) and (x >= 0) do begin
        dec(x, a[i]);
        inc(i);
    end;

    b := intList.Create();
    for j := 1 to n do begin
        read(v);
        b.add(v);
    end;
    readln;
    b.Sort(specialize TComparer<int32>.Construct(@intCompare));

    j := 0;
    while (j < n) and (y >= 0) do begin
        dec(y, b[j]);
        inc(j);
    end;

    writeln(min(i, j));
end.

```
