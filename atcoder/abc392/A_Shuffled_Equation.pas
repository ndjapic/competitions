program A_Shuffled_Equation;
const
    nn = 3;
var
    i: int8;
    p: int32;
    a: array [0 .. nn] of int32;

begin
    p := 1;
    for i := 1 to 3 do begin
        read(a[i]);
        p := p * a[i];
    end;
    readln;

    i := 1;
    while (i <= 3) and (sqr(a[i]) <> p) do inc(i);

    if i <= 3 then
        writeln('Yes')
    else
        writeln('No');
end.
