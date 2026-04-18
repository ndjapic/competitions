program A_Arithmetic_Progression;
var
    a, b, d: int8;

begin
    readln(a, b, d);
    while a < b do begin
        write(a, ' ');
        inc(a, d);
    end;
    writeln(a);
end.
