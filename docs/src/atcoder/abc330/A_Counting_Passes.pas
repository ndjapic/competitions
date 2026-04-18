program A_Counting_Passes;
var
    n, l, i, ai, ans: int16;

begin
    readln(n, l);

    ans := 0;
    for i := 1 to n do begin
        read(ai);
        if ai >= l then inc(ans);
    end;
    readln;

    writeln(ans);
end.
