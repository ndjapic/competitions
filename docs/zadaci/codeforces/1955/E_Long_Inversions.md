# Задатак: E_Long_Inversions.pas

```pascal
program E_Long_Inversions;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci: int16;
    n, k, i: int16;
    s: string;
    found: boolean;
    b, b0: array of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        setlength(b, n+1);
        setlength(b0, n+1);
        for i := 1 to n do b0[i] := s[i] = '0';

        found := false;
        k := n+1;
        while not found do begin

            dec(k);
            b := b0;
            for i := n-k downto 1 do b[i] := b[i] xor b[i+k];

            i := 2;
            while (i <= k) and (b[i] = b[1]) do inc(i);
            found := i > k;

        end;

        writeln(k);

    end;
end.

```
