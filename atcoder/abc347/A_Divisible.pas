program A_Divisible;
const
    maxn = 100;
var
    n, i, k, x: int8;

begin
    readln(n, k);
    for i := 1 to n do begin
        read(x);
        if x mod k = 0 then write(x div k, ' ');
    end;
    readln;
    writeln;
end.
