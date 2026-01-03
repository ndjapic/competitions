program A_Constructive_Problems;
uses
    math;
var
    ntc, tci: int16;
    n, m: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n, m);
        writeln(max(n, m));
    end;
end.
