# Problem: C_Combination_Lock.pas

```pascal
program C_Combination_Lock;
var
    ntc, tci: int16;
    n, i: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        if odd(n) then begin
            for i := 1 to n do write((i-1) * 2 mod n + 1, ' ');
            writeln;
        end else
            writeln(-1);

    end;
end.

```
