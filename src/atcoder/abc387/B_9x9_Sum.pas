program B_9x9_Sum;
var
    x, i, j, ans: int32;

begin
    readln(x);

    ans := 0;

    for i := 1 to 9 do
        for j := 1 to 9 do
            if i*j <> x then inc(ans, i*j);

    writeln(ans);
end.
