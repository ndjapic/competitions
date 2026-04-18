# Задатак: C_Quests.pas

```pascal
program C_Quests;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci, m: int16;
    n, k, i, s, xp: int32;
    a, b: array [1 .. maxn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do read(b[i]); readln;

        s := 0;
        m := 0;
        xp := 0;

        for i := 1 to min(n, k) do begin
            inc(s, a[i]);
            m := max(m, b[i]);
            xp := max(xp, s + (k-i) * m);
        end;

        writeln(xp);

    end;
end.

```
