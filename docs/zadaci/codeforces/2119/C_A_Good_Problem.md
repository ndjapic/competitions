# Задатак: C_A_Good_Problem.pas

```pascal
program C_A_Good_Problem;
uses
    math;
var
    ntc, tci: int32;
    n, l, r, k, p2: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, l, r, k);

        if odd(n) then
            writeln(l)
        else if n = 2 then
            writeln(-1)
        else begin

            p2 := 1;
            while p2 <= l do p2 := p2 * 2;

            if p2 > r then
                writeln(-1)
            else if k <= n-2 then
                writeln(l)
            else
                writeln(p2);

        end;

    end;
end.

```
