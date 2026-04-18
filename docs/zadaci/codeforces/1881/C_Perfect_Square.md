# Задатак: C_Perfect_Square.pas

```pascal
program C_Perfect_Square;
uses
    math;
const
    maxn = 1000;
var
    ntc, tci, m: int8;
    n, h, i, j: int16;
    ch: char;
    ans: int32;
    a: array [1 .. maxn, 1 .. maxn] of int8;
    b: array [1 .. 4] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        h := n div 2;

        for i := 1 to n do begin
            for j := 1 to n do begin
                read(ch);
                a[i, j] := ord(ch) - ord('a');
            end;
            readln;
        end;

        ans := 0;
        for i := 1 to h do
            for j := 1 to h do begin
                b[1] := a[i, j];
                b[2] := a[j, n+1-i];
                b[3] := a[n+1-i, n+1-j];
                b[4] := a[n+1-j, i];
                m := max(
                    max(b[1], b[2]),
                    max(b[3], b[4])
                );
                inc( ans, m-b[1] + m-b[2] + m-b[3] + m-b[4] );
            end;

        writeln(ans);

    end;
end.

```
