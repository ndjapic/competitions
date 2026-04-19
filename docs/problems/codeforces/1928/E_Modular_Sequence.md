# Problem: E_Modular_Sequence.pas

```pascal
program E_Modular_Sequence;
const
    maxn = 200 * 1000;
var
    ntc, tci, n, x, y, i, m: int32;
    s: int64;
    a: array [1 .. maxn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x, y, s);

        m := x mod y;
        a[1] := x - m;
        dec(s, a[1] + int64(n) * m);

        for i := 2 to n do begin
            a[i] := a[i-1] + y;
            if s < a[i] then a[i] := 0;
            dec(s, a[i]);
        end;

        if s <> 0 then
            writeln('No')
        else begin
            writeln('Yes');
            for i := 1 to n-1 do write(a[i] + m, ' ');
            writeln(a[n] + m);
        end;

    end;
end.

```
