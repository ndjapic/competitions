program B_Minimize_Abs_1;
uses
    math;
var
    n, i, l, r, x: int32;

begin
    readln(n, l, r);

    for i := 1 to n do begin
        read(x);
        x := max(x, l);
        x := min(x, r);
        write(x, ' ');
    end;
    readln;
    writeln;
end.
