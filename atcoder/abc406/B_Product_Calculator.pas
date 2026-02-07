program B_Product_Calculator;
var
    n, k, i: int8;
    limit, ans, x: int64;

begin
    readln(n, k);

    limit := 1;
    for i := 1 to k do limit := limit * 10;
    dec(limit);

    ans := 1;
    for i := 1 to n do begin
        read(x);
        if x <= limit div ans then
            ans := ans * x
        else
            ans := 1;
    end;
    readln;

    writeln(ans);
end.
