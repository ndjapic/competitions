# Problem: A_Permutation_Warm_Up.pas

```pascal
program A_Permutation_Warm_Up;
var
    ntc, tci: int16;
    n, h: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
        h := n div 2;
        writeln(h * (n-h) + 1);

    end;
end.

```
