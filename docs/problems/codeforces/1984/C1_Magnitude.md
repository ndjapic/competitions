# Problem: C1_Magnitude.pas

```pascal
program C1_Magnitude;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    s1, s2, s3, s4: int64;
    a: array [1 .. nn] of int32;
    mn, mx: array [0 .. nn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        mn[0] := 0;
        mx[0] := 0;
        for i := 1 to n do begin

            read(a[i]);

            s1 := mn[i-1] + a[i];
            s2 := mx[i-1] + a[i];
            s3 := abs(s1);
            s4 := abs(s2);

            mn[i] := s1;
            mx[i] := max(s3, s4);

        end;
        readln;

        writeln(mx[n]);

    end;
end.

```
