# Problem: C_Manhattan_Permutations.pas

```pascal
program C_Manhattan_Permutations;
{$mode objfpc}{$H+}{$J-}
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, l, r, m: int32;
    k: int64;
    ans: boolean;
    s: string;
    p: array [1 .. nn] of int32;

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

        readln(n, k);

        ans := not odd(k);
        if ans then begin

            k := k div 2;
            l := 1;
            r := n;

            while r-l >= 0 do begin

                if (k > 0) and (r-l = k+1) then begin
                    p[r] := r;
                    dec(l);
                end else if k >= r-l then begin
                    p[l] := r;
                    p[r] := l;
                    dec(k, r-l);
                end else begin
                    p[l] := l;
                    p[r] := r;
                end;

                inc(l);
                dec(r);

            end;

            ans := k = 0;

        end;

        if ans then begin

            writeln('Yes');

            setlength(s, n*11);
            m := 0;
            for i := 1 to n do begin
                writeint(p[i]);
                inc(m);
                s[m] := ' ';
            end;

            setlength(s, m-1);
            writeln(s);

        end else
            writeln('No');

    end;
end.

```
