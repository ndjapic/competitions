# Problem: A_Olympiad_Date.pas

```pascal
program A_Olympiad_Date;
{$H+}
uses
    math;
var
    ntc, tci: int16;
    n, i, d, ai, mx, ans: int8;
    c: array [0 .. 9] of int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for d := 0 to 9 do c[d] := 0;
        s := '01032025';
        for i := 1 to 8 do
            inc(c[ord(s[i]) - ord('0')]);

        ans := 0;
        for i := 1 to n do begin
            read(ai);
            if c[ai] > 0 then begin
                dec(c[ai]);
                mx := 0;
                for d := 0 to 9 do mx := max(mx, c[d]);
                if mx = 0 then ans := i;
            end;
        end;
        readln;

        writeln(ans);

    end;
end.

```
