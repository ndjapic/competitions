program B_Delimiter;
const
    maxn = 100;
var
    n, i: int8;
    a: array [1 .. maxn] of int32;

begin
    n := 1;
    readln(a[1]);

    while a[n] > 0 do begin
        inc(n);
        readln(a[n]);
    end;

    for i := n downto 1 do writeln(a[i]);
end.
