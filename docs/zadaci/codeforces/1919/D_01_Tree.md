# Задатак: D_01_Tree.pas

```pascal
program D_01_Tree;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        for i := 1 to n do read(a[i]); readln;

        writeln('YES');
        writeln('NO');

    end;
end.

```
