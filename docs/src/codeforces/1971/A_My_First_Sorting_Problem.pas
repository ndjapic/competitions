program A_My_First_Sorting_Problem;
var
    ntc, tci, x, y: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(x, y);
        if x < y then
            writeln(x, ' ', y)
        else
            writeln(y, ' ', x);
    end;
end.
