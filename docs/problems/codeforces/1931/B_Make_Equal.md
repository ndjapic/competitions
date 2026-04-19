# Problem: B_Make_Equal.pas

```pascal
program B_Make_Equal;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, m, d: int32;
    s: int64;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        s := 0;
        for i := 1 to n do begin
            read(a[i]);
            inc(s, a[i]);
        end;
        readln;

        m := s div n;

        i := n;
        s := 0;
        while (i > 0) and (s >= 0) do begin

            d := max(0, a[i]-m);
            dec(s, d);
            dec(a[i], d);

            d := max(0, m-a[i]);
            inc(s, d);
            inc(a[i], d);

            dec(i);

        end;

        if s >= 0 then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
