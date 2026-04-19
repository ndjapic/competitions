# Problem: B_Fireworks.pas

```pascal
program B_Fireworks;
var
    ntc, tci: int16;
    a, b, m: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, m);
        writeln(m div a + m div b + 2);

    end;
end.

```
