program B_Integer_Division_Returns;
var
    x: int64;

begin
    readln(x);
    if x >= 0 then
        writeln((x + 9) div 10)
    else
        writeln(-((-x) div 10));
end.
