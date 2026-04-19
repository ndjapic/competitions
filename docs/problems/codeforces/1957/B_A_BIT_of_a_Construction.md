# Problem: B_A_BIT_of_a_Construction.pas

```pascal
program B_A_BIT_of_a_Construction;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, k, i: int32;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        a[1] := 1;
        while 2*a[1]+1 <= k do a[1] := 2*a[1]+1;

        for i := 2 to n do a[i] := 0;
        inc(a[n], k-a[1]);

        for i := 1 to n-1 do write(a[i], ' ');
        writeln(a[n]);

    end;
end.

```
