program A_Scary_Fee;
var
    x, c, d: int32;

begin
    readln(x, c);
    d := x div (1000 + c);
    writeln(d * 1000);
end.
