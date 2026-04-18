program B_Arranging_Cats;
{$H+}
uses
    math;
var
    ntc, tci: int16;
    n, i, c01, c10: int32;
    s, f: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);
        readln(f);

        c01 := 0;
        c10 := 0;
        for i := 1 to n do
            if s[i] = '0' then begin
                if f[i] = '1' then inc(c01);
            end else if f[i] = '0' then
                inc(c10);

        writeln(max(c01, c10));

    end;
end.
