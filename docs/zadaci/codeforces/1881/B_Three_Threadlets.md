# Задатак: B_Three_Threadlets.pas

```pascal
program B_Three_Threadlets;
uses
    math;
var
    ntc, tci: int16;
    n, a, b, c, t: int64;
    found: boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, c);

        n := 3;
        found := false;
        while (n <= 6) and not found do begin
            t := a+b+c;
            found := t mod n = 0;
            if found then begin
                t := t div n;
                found := (a mod t = 0) and (b mod t = 0) and (c mod t = 0);
            end;
            inc(n);
        end;

        if found then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
