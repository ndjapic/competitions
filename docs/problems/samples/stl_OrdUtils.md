# Problem: stl_OrdUtils.pas

```pascal
program stl_OrdUtils;
uses
    gvector, garrayutils, gutil;
type
    iLess = specialize TLess<int32>;
    iVector = specialize TVector<int32>;
    iOrdUtils = specialize TOrderingArrayUtils<iVector, int32, iLess>;
    iUtils = specialize TArrayUtils<iVector, int32>;
var
    V : iVector;
    n, i : int32;

begin
    read(n);
    V := iVector.Create;
    for i := 0 to n-1 do V.PushBack(i);

    iUtils.RandomShuffle(V, n);
    for i := 0 to n-1 do write(' ', V[i]); writeln;

    iOrdUtils.Sort(V, n);
    for i := 0 to n-1 do write(' ', V[i]); writeln;
end.

(*
10
 4 8 6 9 7 2 3 1 5 0
 0 1 2 3 4 5 6 7 8 9
*)

```
