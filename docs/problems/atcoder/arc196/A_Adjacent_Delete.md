# Problem: A_Adjacent_Delete.pas

```pascal
program A_Adjacent_Delete;
uses
    math;
const
    nn = 300 * 1000;
var
    n, l, r: int32;
    a: array [1 .. nn] of int32;
    dp1, dp2: array [0 .. nn] of int64;

begin
    readln(n);
    dp1[0] := 0;
    dp2[0] := 0;

    for r := 1 to n do begin

        read(a[r]);
        l := r mod 2;

        dp1[r] := 0;
        while l < r do begin
            dp1[l] := dp1[l+1] + abs(a[l+1] - a[r]);
            inc(l, 2);
        end;

        dp2[r] := dp2[r-1];
        while l-2 >= 0 do begin
            dec(l, 2);
            dp2[r] := max(dp2[r], dp2[l] + dp1[l]);
        end;

    end;
    readln;

    writeln(dp2[n]);
end.

```
