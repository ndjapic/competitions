# Задатак: B_Move_to_the_End.pas

```pascal
program B_Move_to_the_End;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    s: int64;
    a: array [1 .. nn] of int32;
    m: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        m[0] := 0;
        for i := 1 to n do begin
            read(a[i]);
            m[i] := max(m[i-1], a[i]);
        end;
        readln;

        s := 0;
        for i := n downto 2 do begin
            write(s + m[i], ' ');
            inc(s, a[i]);
        end;
        writeln(s + m[1]);

    end;
end.

```
