# Problem: A_Recovering_a_Small_String.pas

```pascal
program A_Recovering_a_Small_String;
uses
    math;
var
    ntc, tci, n, i: int8;
    c: array [1 .. 3] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 3 downto 1 do begin
            c[i] := min(26, n-(i-1));
            dec(n, c[i]);
        end;

        for i := 1 to 3 do write(chr(ord('a') - 1 + c[i]));
        writeln;

    end;
end.

```
