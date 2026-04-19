# Problem: C_Yarik_and_Array.pas

```pascal
program C_Yarik_and_Array;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, t, i, j, l, r, cs, ans: int32;
    a: array [1 .. maxn] of int32;
    s: array [0 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;

        s[0] := 0;
        t := 0;

        for i := 1 to n do
            if (i = n) or not odd(abs(a[i+1] - a[i])) then begin
                inc(t);
                s[t] := i;
            end;

        ans := low(int32);

        for j := 1 to t do begin
            l := s[j-1] + 1;
            r := s[j];
            cs := 0;
            for i := l to r do begin
                cs := max(a[i], cs + a[i]);
                ans := max(ans, cs);
            end;
        end;

        writeln(ans);

    end;
end.

```
