# Problem: C_Torn_Lucky_Ticket.pas

```pascal
program D_XOR_Construction;
uses
    math;
const
    maxn = 200 * 1000;
var
    {ntc,} tci, n, i, mx: int32;
    a, b: array [1 .. maxn] of int32;

begin
    {readln(ntc);}
    for tci := 1 to 1 do begin

        readln(n);

        b[1] := 0;
        mx := 0;

        for i := 1 to n-1 do begin
            read(a[i]);
            b[i+1] := b[i] xor a[i];
            mx := max(mx, b[i+1]);
        end;
        readln;

        b[1] := mx xor (n-1);
        for i := 1 to n-1 do begin
            write(b[i], ' ');
            b[i+1] := b[i] xor a[i];
        end;
        writeln(b[n]);

    end;
end.

```
