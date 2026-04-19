# Problem: B_Minimise_Sum.pas

```pascal
program B_Minimise_Sum;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i: int32;
    ans: int64;
    a, mn, d: array [0 .. nn] of int32;
    s: array [0 .. nn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        a[0] := 2*n;
        d[0] := 2*n;
        mn[0] := 2*n;
        s[0] := 0;

        for i := 1 to n do begin
            read(a[i]);
            mn[i] := min(mn[i-1], a[i]);
            d[i] := min(d[i-1], max(mn[i-1] - mn[i], 0));
            s[i] := s[i-1] + mn[i];
        end;
        readln;

        ans := s[n];

        for i := 1 to n-1 do
            ans := min(ans, s[i] + min(d[i], a[i+1]));

        writeln(ans);

    end;
end.

```
