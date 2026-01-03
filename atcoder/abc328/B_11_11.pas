program B_11_11;
var
    n, i, j, rd, ans: int32;
    d: array [1 .. 100] of int8;

function repdig(x: int8): int8;
begin
    if x < 10 then
        repdig := x
    else if x div 10 = x mod 10 then
        repdig := x div 10
    else
        repdig := 0;
end;

begin
    readln(n);

    ans := 0;
    for i := 1 to n do begin
        read(d[i]);
        rd := repdig(i);

        if rd > 0 then
            for j := 1 to d[i] do
                if repdig(j) = rd then inc(ans);

    end;
    readln;

    writeln(ans);
end.
