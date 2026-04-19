# Problem: D_Neutral_Tonality.pas

```pascal
program D_Neutral_Tonality;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, k, i: int32;
    b: array [1 .. maxn] of int32;
    seen: array [1 .. maxn] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        for i := 1 to n do begin
            read(b[i]);
            seen[i] := false;
        end;
        readln;

        i := n;
        while not seen[i] and (k > 0) and (b[i] <= n) do begin
            seen[i] := true;
            dec(i, b[i]);
            if i < 1 then inc(i, n);
            dec(k);
        end;

        if k = 0 then
            writeln('Yes')
        else if b[i] > n then
            writeln('No')
        else
            writeln('Yes');

    end;
end.

```
