# Problem: E_Min_Max_MEX.pas

```pascal
program E_Min_Max_MEX;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, j, k, x, l, r, m, c: int32;
    a: array [1 .. nn] of int32;
    seen: array [0 .. nn] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, k);
        for i := 1 to n do read(a[i]); readln;

        l := 0;
        r := n+1;

        while r-l > 1 do begin
            m := (l+r) div 2;
            c := 0;
            j := 1;

            for x := 0 to n do seen[x] := false;
            x := 0;

            for i := 1 to n do begin
                if a[i] <= n then seen[a[i]] := true;
                while seen[x] do inc(x);
                if x >= m then begin
                    x := 0;
                    inc(c);
                    while j <= i do begin
                        if a[j] <= n then seen[a[j]] := false;
                        inc(j);
                    end;
                end;
            end;

            if c < k then
                r := m
            else
                l := m;

        end;

        writeln(l);
 
    end;
end.

```
