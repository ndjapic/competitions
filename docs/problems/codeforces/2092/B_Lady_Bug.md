# Problem: B_Lady_Bug.pas

```pascal
program B_Lady_Bug;
{$H+}
var
    ntc, tci: int16;
    n, i, c0, c1: int32;
    a, b: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(a);
        readln(b);

        c0 := 0;
        c1 := 0;

        for i := 1 to n do
            if odd(i) then begin
                if a[i] = '0' then inc(c0);
                if b[i] = '0' then inc(c1);
            end else begin
                if a[i] = '0' then inc(c1);
                if b[i] = '0' then inc(c0);
            end;

        if (c0 >= (n+1) div 2) and (c1 >= n div 2) then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
