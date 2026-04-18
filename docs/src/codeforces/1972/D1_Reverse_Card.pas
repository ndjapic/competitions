program D1_Reverse_Card;
var
    ntc, tci: int16;
    n, m, k, b, ans: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        ans := 0;
        for b := 1 to m do
            for k := 1 to (n div b + 1) div b do
                if k * b > 1 then inc(ans);

        writeln(ans);

    end;
end.
