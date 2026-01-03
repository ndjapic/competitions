program D_Arcology_On_Permafrost;
uses
    math;
var
    ntc, tci: int16;
    n, m, k, i: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);
        k := max(k, n div (m+1));

        for i := 0 to n-2 do write(i mod k, ' ');
        writeln((n-1) mod k);

    end;
end.
