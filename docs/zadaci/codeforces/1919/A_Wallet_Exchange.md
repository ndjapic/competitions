# Задатак: A_Wallet_Exchange.pas

```pascal
program A_Wallet_Exchange;
var
    ntc, tci: int16;
    a, b: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(a, b);

        if odd(a+b) then
            writeln('Alice')
        else
            writeln('Bob');

    end;
end.

```
