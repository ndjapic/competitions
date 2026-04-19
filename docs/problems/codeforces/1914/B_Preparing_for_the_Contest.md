# Problem: B_Preparing_for_the_Contest.pas

```pascal
program B_Preparing_for_the_Contest;
const
    maxn = 50;
var
    ntc, tci: int16;
    n, k, i: int32;
    a: array [1 .. maxn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to k do a[i] := i;
        for i := k+1 to n do a[i] := n+k+1-i;

        for i := 1 to n-1 do write(a[i], ' ');
        writeln(a[n]);

    end;
end.

```
