# Задатак: D_Elections.pas

```pascal
program D_Elections;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, i0, m: int32;
    c: int64;
    s: string;
    a, ans: array [1 .. nn] of int32;

procedure writeint(x: int32);
begin
    if x > 0 then begin
        writeint(x div 10);
        inc(m);
        s[m] := chr(ord('0') + x mod 10);
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, c);

        i0 := 1;
        for i := 1 to n do begin
            read(a[i]);
            if a[i0] < a[i] then i0 := i;
            ans[i] := i-1;
        end;
        readln;

        for i := 1 to i0-1 do begin
            inc(c, a[i]);
            if c < a[i0] then begin
                inc(ans[i]);
                ans[i0] := min(ans[i0], i-1);
            end;
        end;

        setlength(s, n*11);
        m := 0;
        for i := 1 to n do begin

            if ans[i] = 0 then begin
                inc(m);
                s[m] := '0';
            end else
                writeint(ans[i]);

            inc(m);
            s[m] := ' ';

        end;

        setlength(s, m-1);
        writeln(s);

    end;
end.

```
