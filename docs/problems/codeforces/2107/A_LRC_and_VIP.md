# Problem: A_LRC_and_VIP.pas

```pascal
program A_LRC_and_VIP;
const
    nn = 100;
var
    ntc, tci: int16;
    n, i, mx, mn: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
        read(a[1]);
        mn := 1;
        mx := 1;

        for i := 2 to n do begin
            read(a[i]);
            if a[i] < a[mn] then mn := i;
            if a[i] > a[mx] then mx := i;
        end;
        readln;

        if mn = mx then
            writeln('No')
        else begin
            writeln('Yes');
            for i := 1 to n do
                if i = mx then
                    write('1 ')
                else
                    write('2 ');
            writeln;
        end;

    end;
end.

```
