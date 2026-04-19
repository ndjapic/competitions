# Problem: B_Christmas_Trees.pas

```pascal
program B_Christmas_Trees;
const
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    a, m, l, r: int64;

begin
    readln(a, m, l, r);
    inc(a, inf);
    inc(l, inf);
    inc(r, inf);
    a := a mod m;
    inc(l, m-a);
    inc(r, m-a);

    writeln(r div m - (l-1) div m);
end.

```
