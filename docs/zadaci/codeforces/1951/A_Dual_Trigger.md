# Задатак: A_Dual_Trigger.pas

```pascal
program A_Dual_Trigger;
{$H+}
const
    maxn = 50;
var
    ntc, tci: int16;
    n, i, t: int8;
    s: string;
    a: array of int8;

begin
    setlength(a, 1);
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        t := 0;
        for i := 1 to n do
            if s[i] = '1' then begin
                if length(a) = t then setlength(a, 2*t);
                a[t] := i;
                inc(t);
            end;

        if odd(t) then
            writeln('NO')
        else if t <> 2 then
            writeln('YES')
        else if a[1] - a[0] = 1 then
            writeln('NO')
        else
            writeln('YES');

    end;
end.

```
