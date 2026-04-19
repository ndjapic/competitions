# Problem: C_Cost_to_Flip.pas

```pascal
program C_Cost_to_Flip;
{$mode delphi}
uses
    Generics.Defaults, Generics.Collections;
const
    nn = 200 * 1000;
type
    TRec = record
        i, x: int32;
    end;
var
    n, i, rank, x: int32;
    ans: int64;
    a, b: array [1 .. nn] of int8;
    c: array [1 .. nn] of int32;
    p: TList<int32>;

function Comparison(constref Left, Right: int32): int32;
begin
    Result := - c[Left] + c[Right];
end;

begin
    readln(n);

    for i := 1 to n do read(a[i]); readln;
    for i := 1 to n do read(b[i]); readln;
    for i := 1 to n do read(c[i]); readln;

    p := TList<int32>.Create(TComparer<int32>.Construct(@Comparison));
    for i := 1 to n do p.Add(i);

    for i := 0 to n-1 do write(' ', c[p[i]]); writeln;

    {p.Sort;}
    p.Sort;

    for rank := 0 to n-1 do write(' ', c[p[rank]]); writeln;
end.

```
