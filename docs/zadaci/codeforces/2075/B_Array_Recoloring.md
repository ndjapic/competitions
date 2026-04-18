# Задатак: B_Array_Recoloring.pas

```pascal
program B_Array_Recoloring;
{$MODE DELPHI}
uses
    math, Generics.Defaults, Generics.Collections;
const
    nn = 5000;
type
    TIntComparer = class(TComparer<int32>)
        function Compare(constref Left, Right: int32): Integer; override;
    end;
var
    ntc, tci: int16;
    n, k, i: int32;
    ans: int64;
    a: array [1 .. nn] of int32;
    red: array [1 .. nn] of boolean;
    Comparer: TIntComparer;
    p: TList<int32>;

function TIntComparer.Compare(constref Left, Right: int32): Integer;
begin
    Result := - a[Left] + a[Right];
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        Comparer := TIntComparer.Create;
        Comparer._AddRef;
        p := TList<int32>.Create(Comparer);

		readln(n, k);
        for i := 1 to n do begin
            read(a[i]);
            p.Add(i);
            red[i] := true;
        end;
        readln;
        p.Sort;

        for i := 0 to k do red[p[i]] := false;

        if (k = 1) and red[1] and red[n] then
            ans := a[p[0]] + max(a[1], a[n])
        else begin
            ans := 0;
            for i := 0 to k do inc(ans, a[p[i]]);
        end;

        writeln(ans);

        p.Free;
        Comparer._Release;

    end;

end.

```
