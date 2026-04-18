# Задатак: C_Need_More_Arrays.pas

```pascal
program C_Need_More_Arrays;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, x, ans: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;

        ans := 1;
        x := a[1];

        for i := 2 to n do
            if x+1 < a[i] then begin
                x := a[i];
                inc(ans);
            end;

        writeln(ans);

    end;
end.

```
