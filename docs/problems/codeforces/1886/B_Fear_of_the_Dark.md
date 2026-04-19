# Problem: B_Fear_of_the_Dark.pas

```pascal
program B_Fear_of_the_Dark;
uses
    math;
type
    tpoint = record
        x, y: int32;
    end;
var
    ntc, tci: int16;
    o, p, a, b: tpoint;
    w: real;

function d(a, b: tpoint): real;
begin
    d := sqrt(sqr(a.x-b.x) + sqr(a.y-b.y));
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        o.x := 0;
        o.y := 0;
        readln(p.x, p.y);
        readln(a.x, a.y);
        readln(b.x, b.y);

        if (d(a, o) < d(b, o)) and (d(a, p) < d(b, p)) then
            w := max(d(a, o), d(a, p))
        else if (d(a, o) > d(b, o)) and (d(a, p) > d(b, p)) then
            w := max(d(b, o), d(b, p))
        else
            w := max(
                max(
                    min(d(a, o), d(b, o)),
                    min(d(a, p), d(b, p))
                ), d(a, b) / 2
            );

        writeln(w:8:6);

    end;
end.

```
