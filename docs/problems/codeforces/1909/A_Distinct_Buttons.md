# Problem: A_Distinct_Buttons.pas

```pascal
program A_Distinct_Buttons;
uses
    math;
const
    maxn = 100;
var
    ntc, tci: int16;
    n, i: int32;
    u, r, d, l: boolean;
    x, y: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        u := false;
        r := false;
        d := false;
        l := false;

        for i := 1 to n do begin
            readln(x, y);
            if x > 0 then begin

                r := true;

                if y > 0 then
                    u := true
                else if y < 0 then
                    d := true;

            end else if x < 0 then begin

                l := true;

                if y > 0 then
                    u := true
                else if y < 0 then
                    d := true;

            end else begin

                if y > 0 then
                    u := true
                else if y < 0 then
                    d := true;

            end;
        end;

        if u and r and d and l then
            writeln('NO')
        else
            writeln('YES');

    end;
end.

```
