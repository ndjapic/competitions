# Problem: E_Vlad_and_an_Odd_Ordering.pas

```pascal
program E_Vlad_and_an_Odd_Ordering;
var
    ntc, tci: int32;
    n, k, p2, c: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        p2 := 1;
        c := (n div p2 + 1) div 2;
        while k > c do begin
            dec(k, c);
            p2 := 2 * p2;
            c := (n div p2 + 1) div 2;
        end;

        writeln((2*k-1) * p2);

    end;
end.

```
