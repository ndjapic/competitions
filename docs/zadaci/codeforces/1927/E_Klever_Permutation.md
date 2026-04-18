# Задатак: E_Klever_Permutation.pas

```pascal
program E_Klever_Permutation;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, k, i, j, l, r: int32;
    p: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        l := 1;
        r := n;

        j := 1;
        while j <= k do begin
            i := j;
            while i <= n do begin
                p[i] := r;
                dec(r);
                if i+1 <= n then begin
                    p[i+1] := l;
                    inc(l);
                end;
                inc(i, k);
            end;
            inc(j, 2);
        end;

        for i := 1 to n-1 do write(p[i], ' ');
        writeln(p[n]);

    end;
end.

```
