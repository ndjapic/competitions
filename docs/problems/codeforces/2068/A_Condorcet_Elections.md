# Problem: A_Condorcet_Elections.pas

```pascal
program A_Condorcet_Elections;
uses
    math;
var
    ntc, tci: int16;
    n, i, ai: int32;
    ps, ns, mx: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        ps := 0;
        ns := 0;
        mx := 0;

        for i := 1 to n do begin
            read(ai);
            if ai > 0 then
                inc(ps, ai)
            else
                dec(ns, ai);
            mx := max(mx, ps - ns);
        end;
		readln;

        writeln(mx + ns);

    end;
end.

```
