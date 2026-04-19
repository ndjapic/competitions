# Problem: E_Eat_the_Chip.pas

```pascal
program E_Eat_the_Chip;
uses
    math;
var
    ntc, tci: int16;
    h, w, xa, ya, xb, yb, m: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(h, w, xa, ya, xb, yb);

        if xa >= xb then
            writeln('Draw')
        else if odd(xb-xa) then begin

            m := (xb - xa) div 2;

            inc(xa);
            if ya < yb then
                inc(ya)
            else if ya > yb then
                dec(ya);

            if ya < yb then begin
                ya := min(w, ya+m);
                yb := min(w, yb+m);
            end else if ya > yb then begin
                ya := max(1, ya-m);
                yb := max(1, yb-m);
            end;

            if ya = yb then
                writeln('Alice')
            else
                writeln('Draw');

        end else begin

            m := (xb - xa) div 2;

            if ya > yb then begin
                ya := min(w, ya+m);
                yb := min(w, yb+m);
            end else if ya < yb then begin
                ya := max(1, ya-m);
                yb := max(1, yb-m);
            end;

            if ya = yb then
                writeln('Bob')
            else
                writeln('Draw');

        end;

    end;
end.

```
