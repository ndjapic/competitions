program A_369;
var
    a, b: int8;

begin
    readln(a, b);

    if a = b then
        writeln(1)
    else if odd(a+b) then
        writeln(2)
    else
        writeln(3);
end.
