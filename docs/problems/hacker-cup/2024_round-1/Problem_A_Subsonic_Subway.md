# Problem: Problem_A_Subsonic_Subway.pas

```pascal
program Problem_A_Subsonic_Subway;
var
    ntc, tci: int32;
    n, i, a, b: int32;
    v, mn, mx: extended;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        mx := 0.0;
        mn := -1.0;

        for i := 1 to n do begin

            readln(a, b);

            if a > 0 then begin
                v := 1.0;
                v := v * i / a;
                if (mn < 0.0) or (mn > v) then mn := v;
            end;

            v := 1.0;
            v := v * i / b;
            if mx < v then mx := v;

        end;

        write('Case #', tci, ': ');
        if (mn < 0.0) or (mx <= mn) then
            writeln(mx)
        else
            writeln(-1);

    end;
end.

```
