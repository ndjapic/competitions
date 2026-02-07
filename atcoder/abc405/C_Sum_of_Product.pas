program C_Sum_of_Product;
var
    n, i, x: int32;
    s, s2: int64;

begin
    readln(n);

    s := 0;
    s2 := 0;
    for i := 1 to n do begin
        read(x);
        inc(s, x);
        inc(s2, sqr(x));
    end;
    readln;

    writeln((sqr(s) - s2) div 2);
end.
