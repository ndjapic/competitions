program A_2UP3DOWN;
var
    x, y: int8;

begin
    readln(x,y);
    if (y <= x+2) and (y >= x-3) then
        writeln('Yes')
    else
        writeln('No');
end.
