# Problem: C_Equilateral_Triangle.pas

```pascal
program C_Equilateral_Triangle;
const
    nn = 300 * 1000;
var
    n, l, i, d, s: int32;
    ans: int64;
    c: array [0 .. nn] of int32;

begin
    readln(n, l);

    for s := 0 to l-1 do c[s] := 0;
    s := 0;
    inc(c[s]);

    for i := 1 to n-1 do begin
        read(d);
        inc(s, d);
        s := s mod l;
        inc(c[s]);
    end;
    readln;

    ans := 0;

    if l mod 3 = 0 then begin

        l := l div 3;
        for s := 0 to l-1 do
            inc(ans, int64(c[s]) * c[s+l] * c[s+2*l]);

    end;

    writeln(ans);
end.

```
