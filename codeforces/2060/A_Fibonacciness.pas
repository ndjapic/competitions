program A_Fibonacciness;
const
    nn = 5;
var
    ntc, tci: int16;
    x1, x2, x3: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a[1], a[2], a[4], a[5]);

        x1 := a[1] + a[2];
        x2 := a[4] - a[2];
        x3 := a[5] - a[4];

        if (x1 = x2) and (x2 = x3) then
            writeln(3)
        else if (x1 <> x2) and (x2 <> x3) and (x3 <> x1) then
            writeln(1)
        else
            writeln(2);

    end;
end.
