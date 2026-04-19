# Problem: A_Binary_Imbalance.pas

```pascal
program A_Binary_Imbalance;
const
    maxn = 100;
var
    ntc, tci, n, i, n1: int8;
    s: array [1 .. maxn] of char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);

        n1 := 0;
        for i := 1 to n do begin
            read(s[i]);
            inc(n1, ord(s[i]) - ord('0'));
        end;
        readln;

        if n1 < n then
            writeln('YES')
        else
            writeln('NO');
    end;
end.

```
