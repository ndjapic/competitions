# Problem: C_Sending_Messages.pas

```pascal
program C_Sending_Messages;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, a, b: int32;
    f: int64;
    m: array [0 .. maxn] of int32;

begin
    m[0] := 0;
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, f, a, b);
        for i := 1 to n do read(m[i]); readln;

        for i := 1 to n do begin
            dec(f, min(int64(m[i] - m[i-1]) * a, b));
            f := max(f, 0);
        end;

        if f > 0 then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
