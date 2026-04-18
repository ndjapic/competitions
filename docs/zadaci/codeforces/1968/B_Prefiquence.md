# Задатак: B_Prefiquence.pas

```pascal
program B_Prefiquence;
{$H+}
var
    ntc, tci: int16;
    n, m, i, k: int32;
    a, b: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        readln(a);
        readln(b);

        k := 1;
        for i := 1 to m do
            if (k <= n) and (a[k] = b[i]) then inc(k);

        writeln(k-1);

    end;
end.

```
