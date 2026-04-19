# Problem: C_Two_Colors.pas

```pascal
program C_Two_Colors;
{$MODE DELPHI}
uses
    math, Generics.Defaults, Generics.Collections;
const
    nn = 200 * 1000;
type
    TIntComparer = class(TComparer<int32>)
        function Compare(constref Left, Right: int32): Integer; override;
    end;
var
    ntc, tci: int16;
    n, m, i, l, r, ai: int32;
    ans: int64;
    Comparer: TIntComparer;
    a: TList<int32>;
    s: array [0 .. nn] of int64;

function TIntComparer.Compare(constref Left, Right: int32): Integer;
begin
    Result := Left - Right;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        Comparer := TIntComparer.Create;
        Comparer._AddRef;
        a := TList<int32>.Create(Comparer);

		readln(n, m);

        for i := 0 to m-1 do begin
            read(ai);
            a.Add(min(ai, n-1));
        end;
        readln;
        a.Sort;

        s[0] := 0;
        for i := 0 to m-1 do s[i+1] := s[i] + a[i];

        ans := 0;
        l := 0;
        r := m-1;
        while l < r do
            if a[l] + a[r] < n then
                inc(l)
            else begin
                inc(ans, s[r]-s[l] + int64(r-l) * (a[r]-n+1));
                dec(r);
            end;

        writeln(ans * 2);

        a.Free;
        Comparer._Release;

    end;
end.

```
