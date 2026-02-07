program A_Penalty_Kick;
var
    n, i: int8;

begin
    readln(n);
    for i := 1 to n do
        if i mod 3 = 0 then
            write('x')
        else
            write('o');
    writeln;
end.
