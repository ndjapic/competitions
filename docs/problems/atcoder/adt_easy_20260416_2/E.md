# Problem: E.pas

```pascal
program _E;
uses
    math;
const
    nn = 300 * 1000;
var
    n, i, na, nb, l, r: int32;
    ans: int64;
    p, a, b: array [1 .. nn] of int32;

begin
    readln(n);

    for i := 1 to n do read(p[i]);
    readln;

    ans := 0;
    na := 1;
    nb := 1;
    a[na] := n;
    b[nb] := n;

    for i := n-1 downto 2 do begin

        if (p[i-1] < p[i]) and (p[i] > p[i+1]) then begin
            inc(na);
            a[na] := i;
        end;

        if (p[i-1] > p[i]) and (p[i] < p[i+1]) then begin
            inc(nb);
            b[nb] := i;
        end;

        if (p[i-1] < p[i]) and (na > 1) and (nb > 1) then begin
            l := max(a[na], b[nb]);
            r := min(a[na-1], b[nb-1]);
            inc(ans, max(0, r-l));
        end;

    end;

    writeln(ans);
end.

```
