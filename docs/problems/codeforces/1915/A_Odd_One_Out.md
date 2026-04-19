# Problem: A_Odd_One_Out.pas

```pascal
program A_Odd_One_Out;
var
    ntc, tci: int16;
    a, b, c: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, c);

        if a = b then writeln(c);
        if a = c then writeln(b);
        if c = b then writeln(a);

    end;
end.

```
