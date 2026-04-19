# Problem: A_Contest_Proposal.pas

```pascal
program A_Contest_Proposal;
uses
    math;
const
    sz = 100;
var
    ntc, tci: int8;
    n, i, l, r, ans: int8;
    a, b: array [0 .. sz] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do read(b[i]); readln;

        l := 0;
        a[0] := 0;
        ans := 0;

        for r := 1 to n do begin
            while (l < r) and (a[l+1] <= b[r]) do inc(l);
            ans := max(ans, r-l);
        end;

        writeln(ans);

    end;
end.

```
