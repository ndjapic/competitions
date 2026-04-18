program A_First_ABC_2;
uses
    math;
const
    maxn = 100;
var
    n, i: int8;
    s: array [1 .. maxn] of char;

begin
    readln(n);
    for i := 1 to n do read(s[i]); readln;

    i := 1;
    while (i+2 <= n) and not ((s[i] = 'A') and (s[i+1] = 'B') and (s[i+2] = 'C')) do inc(i);

    if i+2 > n then i := -1;
    writeln(i);
end.

